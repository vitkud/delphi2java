package ru.vitkud.delphi;

import java.util.ArrayList;
import java.util.List;

public class DelphiFunction {
	private String name;
	private boolean classFunction;
	private boolean procedure;
	private DelphiType resultType;
	private List<DelphiArgument> arguments = new ArrayList<>();

	public DelphiFunction(String name) {
		this.name = name;
	}

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
		return arguments.toArray(new DelphiArgument[arguments.size()]);
	}


	public void setClassFunction(boolean classFunction) {
		this.classFunction = classFunction;
	}


	public void setProcedure(boolean procedure) {
		this.procedure = procedure;
	}

	public void setResultType(DelphiType resultType) {
		this.resultType = resultType;
	}

	public void addArgument(DelphiArgument argument) {
		arguments.add(argument);
	}

	public String getName() {
		return name;
	}
}
