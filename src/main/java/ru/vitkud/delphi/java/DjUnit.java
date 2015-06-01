package ru.vitkud.delphi.java;

import ru.vitkud.delphi.DelphiInterface;
import ru.vitkud.delphi.DelphiUnit;

public class DjUnit {

	private DelphiUnit delphiUnit;
	JavaProjectFileAppender projectAppender;
	private String fullPackage;

	public DjUnit(DelphiUnit delphiUnit) {
		this.delphiUnit = delphiUnit;
	}

	public void toJava(JavaProjectFileAppender projectAppender, String basePackage) {
		this.projectAppender = projectAppender;
		this.fullPackage =  basePackage + "." + delphiUnit.getName().toLowerCase();

		for (DelphiInterface delphiInterface : delphiUnit.getInterfaces()) {
			DjInterface djInterface = new DjInterface(delphiInterface);
			djInterface.toJava(projectAppender, fullPackage);
		}
		// TODO ...
	}
}
