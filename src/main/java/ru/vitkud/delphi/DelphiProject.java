package ru.vitkud.delphi;

import java.io.File;
import java.util.List;

public class DelphiProject {
	// TODO: AST -> ASG

	List<DelphiUnit> units;

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
}
