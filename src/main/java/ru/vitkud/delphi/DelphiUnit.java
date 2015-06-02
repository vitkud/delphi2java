package ru.vitkud.delphi;

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.ATNConfigSet;
import org.antlr.v4.runtime.dfa.DFA;
import ru.vitkud.delphi.parser.DelphiLexer;
import ru.vitkud.delphi.parser.DelphiParser;

import java.io.IOException;
import java.io.InputStream;
import java.util.*;

public class DelphiUnit {
	private String name;
	private List<DelphiInterface> interfaces = new ArrayList<>();

	public static class UnitParseError extends Exception {
		int line, posInLine;
		public UnitParseError(String message, int line, int posInLine) {
			super(message);
			this.line = line;
			this.posInLine = posInLine;
		}
		public UnitParseError(String message, Token token) {
			this(message, token.getLine(), token.getCharPositionInLine());
		}
	}

	public DelphiUnit(DelphiProject project, InputStream unitInputStream) throws IOException, UnitParseError {
		// TODO Expand Directives (use: project.defines, poject.searchPaths)
		Set<String> definitionsSet = new HashSet<>(Arrays.asList(project.getDefinitions()));
		ANTLRInputStream input = new ANTLRInputStream(new Precompiler(unitInputStream, definitionsSet));
		// TODO parse unitFile (AST)
		// create a lexer that feeds off of input CharStream
		DelphiLexer lexer = new DelphiLexer(input);

		// create a buffer of tokens pulled from the lexer
		CommonTokenStream tokens = new CommonTokenStream(lexer);

		// create a parser that feeds off the tokens buffer
		DelphiParser parser = new DelphiParser(tokens);

		// TODO handle errors and correct line and pos in line
		//parser.removeErrorListeners();
		//parser.addErrorListener(new BaseErrorListener() {
		//	@Override
		//	public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol, int line, int charPositionInLine, String msg, RecognitionException e) {
		//		System.err.println("line " + line + ":" + charPositionInLine + " " + msg);
		//	}
		//});

		// begin parsing
		DelphiParser.FileContext file = parser.file();

		DelphiParser.UnitContext unit = file.unit();
		if (unit == null)
			throw new UnitParseError("'UNIT' expected but '" + file.getStart().getText(), file.getStart());

		this.name = unit.unitHead().namespaceName().getText();

		// AST -> ASG
		// TODO ...
		// fill interfaces
		fillInterfaces(unit);

	}

	private void fillInterfaces(DelphiParser.UnitContext unit) {
		DelphiParser.UnitInterfaceContext unitInterface = unit.unitInterface();
		if (unitInterface != null) {
			for (DelphiParser.InterfaceDeclContext interfaceDecl : unitInterface.interfaceDecl()) {
				DelphiParser.TypeSectionContext typeSection = interfaceDecl.typeSection();
				if (typeSection != null) {
					for (DelphiParser.TypeDeclarationContext typeDeclaration : typeSection.typeDeclaration()) {
						DelphiParser.TypeDeclContext typeDecl = typeDeclaration.typeDecl();
						if (typeDeclaration.customAttribute() != null) {
							System.err.println("Custom attributes not supported: " + typeDeclaration.customAttribute().getText());
							continue;
						}
						DelphiParser.StrucTypeContext strucType = typeDecl.strucType();
						if (strucType == null) {
							System.err.println("Support only 'strucType': " + typeDecl.getText());
							continue;
						}
						DelphiParser.ClassDeclContext classDecl = strucType.strucTypePart().classDecl();
						if (classDecl == null) {
							System.err.println("Support only 'classDecl': " + typeDecl.getText());
							continue;
						}
						DelphiParser.InterfaceTypeDeclContext interfaceTypeDecl = classDecl.interfaceTypeDecl();
						if (interfaceTypeDecl == null) {
							System.err.println("Support only 'interfaceTypeDecl': " + typeDecl.getText());
							continue;
						}

						DelphiInterface delphiInterface = new DelphiInterface(typeDeclaration.genericTypeIdent().getText());
						DelphiParser.InterfaceGuidContext interfaceGuid = interfaceTypeDecl.interfaceGuid();
						if (interfaceGuid != null)
							delphiInterface.setGuid(interfaceGuid.getText());
						DelphiParser.ClassParentContext classParent = interfaceTypeDecl.classParent();
						if (classParent != null) {
							for (DelphiParser.GenericTypeIdentContext typeIdent : classParent.genericTypeIdent()) {
								delphiInterface.addUnknownParent(typeIdent.getText());
							}
							if (interfaceTypeDecl.interfaceItem() == null) {
								System.err.print("interfaceItem == null");
								continue;
							}
							for (DelphiParser.InterfaceItemContext interfaceItem : interfaceTypeDecl.interfaceItem()) {
								DelphiParser.ClassMethodContext classMethod = interfaceItem.classMethod();
								if (classMethod != null) {
									DelphiParser.IdentContext ident = classMethod.ident();

									DelphiFunction delphiFunction = new DelphiFunction(ident.getText());
									if (classMethod.FUNCTION() != null) {
										delphiFunction.setProcedure(false);
										delphiFunction.setResultType(new DelphiType(classMethod.typeDecl().getText()));
										if (classMethod.CLASS() != null)
											delphiFunction.setClassFunction(true);
									} else {
										delphiFunction.setProcedure(true);
										if (classMethod.methodKey().CLASS() != null)
											delphiFunction.setClassFunction(true);
									}
									DelphiParser.FormalParameterSectionContext formalParameterSection = classMethod.formalParameterSection();
									if (formalParameterSection != null) {
										DelphiParser.FormalParameterListContext formalParameterList = formalParameterSection.formalParameterList();
										if (formalParameterList != null) {
											for (DelphiParser.FormalParameterContext formalParameter : formalParameterList.formalParameter()) {
												for (DelphiParser.IdentContext identPar : formalParameter.identListFlat().ident()) {
													delphiFunction.addArgument(new DelphiArgument(identPar.getText(),
															new DelphiType(formalParameter.typeDecl().getText())));
												}
											}
										}
									}
									delphiInterface.addFunction(delphiFunction);
								} else {
									DelphiParser.ClassPropertyContext classProperty = interfaceItem.classProperty();
									System.err.println("'classProperty' not supported: " + classProperty.getText());
									//continue;
								}
							}
						this.addInterface(delphiInterface);
						}
					}
				}
			}
		}
	}

	private void addInterface(DelphiInterface delphiInterface) {
		interfaces.add(delphiInterface);
	}

	public String getName() {
		return name;
	}

	public DelphiInterface[] getInterfaces() {
		return interfaces.toArray(new DelphiInterface[interfaces.size()]);
	}
}
