package ru.vitkud.delphi;

public class DelphiInterface {
	private DelphiInterface[] parents;
	private String name;
	private DelphiFunction[] functions;

	public DelphiInterface[] getParents() {
		return parents;
	}

	public String getName() {
		return name;
	}

	public DelphiFunction[] getFunctions() {
		return functions;
	}
}
