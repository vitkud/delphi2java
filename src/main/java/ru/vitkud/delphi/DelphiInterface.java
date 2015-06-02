package ru.vitkud.delphi;

import java.util.ArrayList;
import java.util.List;

public class DelphiInterface {
	private List<DelphiInterface> parents = new ArrayList<>();
	private String name;
	private List<DelphiFunction> functions = new ArrayList<>();
	private String guid;

	public DelphiInterface(String name) {
		this.name = name;
	}

	public DelphiInterface[] getParents() {
		return parents.toArray(new DelphiInterface[parents.size()]);
	}

	public String getName() {
		return name;
	}

	public DelphiFunction[] getFunctions() {
		return functions.toArray(new DelphiFunction[functions.size()]);
	}

	public void setGuid(String guid) {
		this.guid = guid;
	}

	public void addUnknownParent(String parentName) {
		parents.add(new DelphiInterface(parentName));
	}

	public void addFunction(DelphiFunction delphiFunction) {
		functions.add(delphiFunction);
	}
}
