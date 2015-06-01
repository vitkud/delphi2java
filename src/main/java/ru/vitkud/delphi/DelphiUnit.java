package ru.vitkud.delphi;

import java.io.File;

public class DelphiUnit {
	private String name;
	private DelphiInterface[] interfaces;

	public DelphiUnit(DelphiProject project, File unitFile) {
		// TODO Expand Directives (use: project.defines, poject.searchPaths)
		// TODO parse unitFile (AST)
		// AST -> ASG
		// TODO ...
		// fill interfaces
		fillInterfaces();

	}

	private void fillInterfaces() {
		// TODO ...
	}

	public String getName() {
		return name;
	}

	public DelphiInterface[] getInterfaces() {
		return interfaces;
	}
}
