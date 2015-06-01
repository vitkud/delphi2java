package ru.vitkud.delphi.java;

import ru.vitkud.delphi.DelphiType;

public class DjType {
	String javaType;

	public DjType(DelphiType delphiType) {
		javaType = delphiType.getName();
		if (javaType.equalsIgnoreCase("string"))
			javaType = "String";
		else if (javaType.equalsIgnoreCase("Integer"))
			javaType = "int";
		// TODO ...
	}

	@Override
	public String toString() {
		return javaType;
	}
}

