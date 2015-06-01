package ru.vitkud.delphi.java;

import ru.vitkud.delphi.DelphiProject;
import ru.vitkud.delphi.DelphiUnit;

import java.io.File;

public class Delphi2Java {
	public Delphi2Java() {
		// TODO ...
	}

	private File outputPath;
	private String basePackage;

	private JavaProjectFileAppender projectAppender;

	public void convert(DelphiProject delphiProject) {
		// TODO ...
		// get all units and assign packages
		// for each unit:
		//  create interfaces
		//  create classes
		//  create 'Common' class for functions and procedures without class

		adjustSettings();

		for (DelphiUnit delphiUnit : delphiProject.getUnits()) {
			DjUnit unit = new DjUnit(delphiUnit);
			unit.toJava(projectAppender, basePackage);
		}

	}

	private void adjustSettings() {
		if (outputPath == null) {
			projectAppender = new JavaProjectFileAppender() {
				@Override
				public void appendMainJava(String fullPackage, String className, String content) {
					System.out.println(fullPackage.replace('.', '/') + "/" + className + ".java");
					System.out.println(content);
				}
				@Override
				public void appendTestJava(String fullPackage, String className, String content) {
					System.out.println(fullPackage.replace('.', '/') + "/" + className + ".java");
					System.out.println(content);
				}
			};
			//outputPath = new File(System.getProperty("user.dir"), "java");
		//} else {
		//	// TODO ...
		}
		if (basePackage == null)
			basePackage = "";
	}

	public void setOutputPath(File outputPath) {
		this.outputPath = outputPath;
	}

	public void setBasePackage(String basePackage) {
		this.basePackage = basePackage;
	}
}
