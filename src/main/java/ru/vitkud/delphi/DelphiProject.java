package ru.vitkud.delphi;

import java.io.File;
import java.util.List;

public class DelphiProject {
	// TODO: AST -> ASG

	List<DelphiUnit> units;
	private String[] definitions;

	public void loadDelphiProject(File dprojFile) {
		// from dprojFile:
		//   get MainSource (project.dpr)
		//   get DCC_Define
		//   get DCC_UnitSearchPath
		// Get AST of MainSource

		// TODO: ...
	}

	public List<DelphiUnit> getUnits() {
		return units;
	}

	public String[] getDefinitions() {
		return definitions;
	}

	public void setDefinitions(String[] definitions) {
		this.definitions = definitions;
	}
}
