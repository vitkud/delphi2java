package ru.vitkud.delphi;

public class DelphiArgument {
	private DelphiType type;
	private String name;

	public DelphiArgument(String name, DelphiType type) {
		this.name = name;
		this.type = type;
	}

	public DelphiType getType() {
		return type;
	}

	public String getName() {
		return name;
	}
}
