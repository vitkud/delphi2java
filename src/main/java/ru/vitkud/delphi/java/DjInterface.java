package ru.vitkud.delphi.java;

import ru.vitkud.delphi.DelphiArgument;
import ru.vitkud.delphi.DelphiFunction;
import ru.vitkud.delphi.DelphiInterface;
import ru.vitkud.delphi.parser.DelphiParser;

import java.io.File;

public class DjInterface {
	private DelphiInterface delphiInterface;

	public DjInterface(DelphiInterface delphiInterface) {
		this.delphiInterface = delphiInterface;
	}

	public void toJava(JavaProjectFileAppender projectAppender, String fullPackage) {
		StringBuilder javaFileCont = new StringBuilder();
		javaFileCont.append("package ").append(fullPackage).append(";\n");
		// TODO adjust imports

		javaFileCont.append("public interface ").append(delphiInterface.getName());

		DelphiInterface[] parents = delphiInterface.getParents();
		if (parents.length > 0) {
			javaFileCont.append(" extends ");
			boolean first = true;
			for (DelphiInterface parent : parents) {
				javaFileCont.append((first ? "" : ", ")).append(parent.getName());
				first = false;
			}
		}

		javaFileCont.append(" {");

		DelphiFunction[] delphiFunctions = delphiInterface.getFunctions();
		for (DelphiFunction delphiFunction : delphiFunctions) {
			javaFileCont.append("\t");
			if (delphiFunction.isClassFunction())
				javaFileCont.append("static ");
			if (delphiFunction.isProcedure()) {
				javaFileCont.append("void ");
			} else {
				javaFileCont.append(new DjType(delphiFunction.getResultType()).toString()).append(" ");
			}

			javaFileCont.append("(");
			DelphiArgument[] arguments = delphiFunction.getArguments();
			for (DelphiArgument argument : arguments) {
				javaFileCont.append(new DjType(argument.getType()).toString()).append(" ").append(argument.getName());
			}

		}

		javaFileCont.append("}\n");
		projectAppender.appendMainJava(fullPackage, delphiInterface.getName(), javaFileCont.toString());
	}
}
