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
		javaFileCont.append("package ").append(fullPackage).append(";\n\n");
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

		javaFileCont.append(" {\n");

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
			javaFileCont.append(DjHelper.delphiNameToJava(delphiFunction.getName())).append("(");
			DelphiArgument[] arguments = delphiFunction.getArguments();
			boolean first = true;
			for (DelphiArgument argument : arguments) {
				if (!first) javaFileCont.append(", ");
				first = false;
				javaFileCont.append(new DjType(argument.getType()).toString()).append(" ")
						.append(DjHelper.delphiNameToJava(argument.getName()));
			}
			javaFileCont.append(");\n");
		}

		javaFileCont.append("}\n\n");
		projectAppender.appendMainJava(fullPackage, delphiInterface.getName(), javaFileCont.toString());
	}
}
