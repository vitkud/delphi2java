package ru.vitkud.delphi;

public class DelphiFunction {
	private boolean classFunction;
	private boolean procedure;
	private DelphiType resultType;
	private DelphiArgument[] arguments;

	public boolean isClassFunction() {
		return classFunction;
	}

	public boolean isProcedure() {
		return procedure;
	}

	public DelphiType getResultType() {
		return resultType;
	}

	public DelphiArgument[] getArguments() {
		return arguments;
	}
}
