package ru.vitkud.delphi;

import org.antlr.v4.runtime.*;
import ru.vitkud.delphi.parser.*;

import java.io.InputStream;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public class Delphi2Java {
    public static void convert(InputStream delphiSrcStream, String[] definitions) throws Exception {
        Set<String> definitionsSet = new HashSet<>(Arrays.asList(definitions));
        ANTLRInputStream input = new ANTLRInputStream(new Precompiler(delphiSrcStream, definitionsSet));

        // create a lexer that feeds off of input CharStream
        DelphiLexer lexer = new DelphiLexer(input);

        // create a buffer of tokens pulled from the lexer
        CommonTokenStream tokens = new CommonTokenStream(lexer);

        // create a parser that feeds off the tokens buffer
        DelphiParser parser = new DelphiParser(tokens);

        // begin parsing
        DelphiParser.FileContext file = parser.file();

        DelphiParser.UnitContext unit = file.unit();
        if (unit != null) {
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
                            DelphiParser.InterfaceGuidContext interfaceGuid = interfaceTypeDecl.interfaceGuid();
                            if (interfaceGuid != null) {
                                System.out.println("// " + interfaceGuid.getText());
                            }
                            DelphiParser.ClassParentContext classParent = interfaceTypeDecl.classParent();
                            System.out.print("interface " + typeDeclaration.genericTypeIdent().getText());
                            if (classParent != null) {
                                System.out.print(" extends ");
                                boolean first = true;
                                for (DelphiParser.GenericTypeIdentContext typeIdent : classParent.genericTypeIdent()) {
                                    System.out.print((first ? "" : ", ") + typeIdent.getText());
                                    first = false;
                                }
                            }
                            System.out.println(" {");
                            if (interfaceTypeDecl.interfaceItem() == null) {
                                System.err.print("interfaceItem == null");
                            } else {
                                for (DelphiParser.InterfaceItemContext interfaceItem : interfaceTypeDecl.interfaceItem()) {
                                    DelphiParser.ClassMethodContext classMethod = interfaceItem.classMethod();
                                    if (classMethod != null) {
                                        System.out.print("\t");
                                        DelphiParser.IdentContext ident = classMethod.ident();
                                        if (classMethod.FUNCTION() != null) {
                                            if (classMethod.CLASS() != null) System.out.print("static ");
                                            System.out.print(classMethod.typeDecl().getText() + " ");
                                        } else {
                                            if (classMethod.methodKey().CLASS() != null) System.out.print("static ");
                                            System.out.print("void ");
                                        }
                                        System.out.print(ident.getText() + "(");
                                        boolean firstPar = true;
                                        DelphiParser.FormalParameterSectionContext formalParameterSection = classMethod.formalParameterSection();
                                        if (formalParameterSection != null) {
                                            DelphiParser.FormalParameterListContext formalParameterList = formalParameterSection.formalParameterList();
                                            if (formalParameterList != null) {
                                                for (DelphiParser.FormalParameterContext formalParameter : formalParameterList.formalParameter()) {
                                                    for (DelphiParser.IdentContext identPar : formalParameter.identListFlat().ident()) {
                                                        if (!firstPar) System.out.print(", ");
                                                        firstPar = false;
                                                        System.out.print(formalParameter.typeDecl().getText() + " " + identPar.getText());
                                                    }
                                                }
                                            }
                                        }
                                        System.out.println(");");
                                    } else {
                                        DelphiParser.ClassPropertyContext classProperty = interfaceItem.classProperty();
                                        System.err.println("'classProperty' not supported: " + classProperty.getText());
                                        //continue;
                                    }
                                }
                            }
                            System.out.println("}");
                            System.out.println("");
                        }
                    }

                }
            }
        }

    }
}
