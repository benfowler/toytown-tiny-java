/*
 *  Toytown -- a reimplementation of Peter Norvig's lis.py interpreter for
 *  Python.
 *
 *  Copyright (C) 2012-2013  Benjamin J. Fowler.
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

package benfowler.toytown.tiny;

import static benfowler.toytown.tiny.Interpreter.read;

import java.io.PrintStream;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.StringTokenizer;

import benfowler.toytown.tiny.err.Errors;
import benfowler.toytown.tiny.err.EvalException;
import benfowler.toytown.tiny.err.ParseException;


/**
 * Implements a simple toy Lisp interpreter.  Inspired by Peter Norvig's
 * lis.py implementation.
 */
public class Interpreter {

	public static Object read(String program) throws EvalException {
		return parse(program);
	}

	public static Object parse(String program) throws EvalException {
		Lexer lexer = new Lexer(program);
		Object result = null;
		if (lexer.peekToken().equals("(")) {
			result = sExpression(lexer);
		} else {
			result = lexer.consumeToken();
		}

		if (lexer.hasMoreTokens()) {
			throw new ParseException(Errors.TOKEN_AFTER_END_OF_PROGRAM);
		}

		return result;
	}

	public static Object eval(Object arg) throws EvalException {
		return eval(arg, Builtins.getGlobalEnvironment());
	}

	public static Object eval(Object arg, Environment env)
	throws EvalException {
		if (arg instanceof Double || arg instanceof Boolean) {  // literal
			return arg;
		} else if (arg instanceof Symbol) {
			Object resolved = env.get(arg.toString());
			if (resolved == null) {
				throw new EvalException(Errors.SYMBOL_NOT_FOUND,
						arg.toString());
			}
			return resolved;
		} else if (arg instanceof List) {
			List<?> args = (List<?>)arg;
			if (args.size() == 0) {
				return args;
			}

			if (args.get(0) instanceof Dot) {
				return dot(args, env);
			} else if (args.get(0) instanceof SpecialForm) {
				SpecialForm sf = (SpecialForm)args.get(0);
				switch(sf) {
				case QUOTE: return quote(args);
				case IF: return if_(args, env);
				case BEGIN: return begin(args, env);
				case SET_: return set_(args, env);
				case DEFINE: return define(args, env);
				case LAMBDA: return lambda(args, env);
				default:
					throw new EvalException(Errors.INTERNAL_ERROR,
							String.format("Unexpected special form: %s",
									args.get(0).toString()));
				}
			} else {
				return proc(args, env);
			}

		} else {
			throw new EvalException(Errors.CANNOT_EVAL,
					arg.getClass().getName(), arg.toString());
		}
	}

	private static List<Object> sExpression(Lexer lexer)
	throws EvalException {
		List<Object> result = new LinkedList<Object>();
		lexer.expect("(");
		while (!lexer.peekToken().equals(")")) {
			if (lexer.peekToken().equals("(")) {
				result.add(sExpression(lexer));
			} else {
				result.add(lexer.consumeToken());
			}

		}
		lexer.expect(")");
		return result;
	}

	private static Object dot(List<?> args, Environment env) {
		if (args.size() < 2) {
			throw new EvalException(Errors.INVALID_NUMBER_OF_ARGUMENTS,
					2, -1, args.size()-1);
		}
		if (!(args.get(1) instanceof Symbol)) {
			throw new EvalException(Errors.INSTANCE_OR_CLASSNAME_EXPECTED,
					args.get(1));
		}

		String methodName = ((Dot)args.get(0)).methodName;
		String instanceOrClassName = ((Symbol)args.get(1)).toString();

		Object instance = null;
		Class<?> instanceClass = null;
		if (env.containsKey(instanceOrClassName)) {
			instance = env.get(instanceOrClassName);
			instanceClass = instance.getClass();
		} else {
			try {
				instanceClass = Class.forName(instanceOrClassName);
			} catch (ClassNotFoundException e) {
				throw new EvalException(
						Errors.INSTANCE_OR_CLASSNAME_NOT_FOUND,
						instanceOrClassName);
			}
		}

		List<Object> argInstances = new LinkedList<Object>();
		List<Class<?>> argClasses = new LinkedList<Class<?>>();
		for (int i=2; i<args.size(); ++i) {
			Object arg = args.get(i);
			Object evaluatedArg = eval(arg, env);
			argInstances.add(evaluatedArg);
			argClasses.add(evaluatedArg.getClass());
		}

		Method method = null;
		try {
			method = instanceClass.getDeclaredMethod(methodName,
					argClasses.toArray(new Class<?>[] {}));

			// Sanity check for static calls
			if (instance == null && !Modifier.isStatic(method.getModifiers())) {
				throw new EvalException(Errors.NONSTATIC_CALL_IN_STATIC_CTX,
						method.toString());
			}

			return method.invoke(instance,
					argInstances.toArray(new Object[] {}));
		} catch (Exception e) {
			throw new EvalException(Errors.BAD_METHOD_INVOCATION,
					e.getClass().getName(), instanceOrClassName, methodName,
					e.getMessage());
		}
	}

	private static Object quote(List<?> args) {
		if (args.size() != 2) {
			throw new EvalException(Errors.INVALID_NUMBER_OF_ARGUMENTS,
					1, 1, args.size()-1);
		}
		return args.get(1);
	}

	private static Object if_(List<?> args, Environment env) {
		if (args.size() < 3 || args.size() > 4) {
			throw new EvalException(Errors.INVALID_NUMBER_OF_ARGUMENTS,
					2, 3, args.size()-1);
		}
		Object cond = eval(args.get(1), env);
		if (!(cond instanceof Boolean)) {
			throw new EvalException(Errors.BAD_IF_CONDITION,
					cond.getClass().getName());
		}
		if (((Boolean)cond).booleanValue()) {  // conseq
			return eval(args.get(2), env);
		} else  {  // alt
			if (args.size() == 4) {
				return eval(args.get(3), env);
			}
		}
		return null;
	}

	private static Object begin(List<?> args, Environment env) {
		if (args.size() < 2) {
			throw new EvalException(Errors.INVALID_NUMBER_OF_ARGUMENTS,
					1, -1, args.size()-1);
		}
		Object result = null;
		for (int i=1; i<args.size(); ++i) {
			result = eval(args.get(i), env);
		}
		return result;
	}

	private static Object set_(List<?> args, Environment env) {
		if (args.size() != 3) {
			throw new EvalException(Errors.INVALID_NUMBER_OF_ARGUMENTS,
					3, 3, args.size()-1);
		}
		if (!(args.get(1) instanceof Symbol)) {
			throw new EvalException(Errors.SYMBOL_EXPECTED);
		}
		String newName = ((Symbol) args.get(1)).toString();
		if (!env.containsKey(newName)) {
			throw new EvalException(Errors.SYMBOL_MUST_BE_DEFINED,
					newName);
		}
		env.put(newName, eval(args.get(2), env));
		return null;
	}

	private static Object define(List<?> args, Environment env) {
		if (args.size() != 3) {
			throw new EvalException(Errors.INVALID_NUMBER_OF_ARGUMENTS,
					3, 3, args.size()-1);
		}
		if (!(args.get(1) instanceof Symbol)) {
			throw new EvalException(Errors.SYMBOL_EXPECTED);
		}
		String name = ((Symbol) args.get(1)).toString();
		env.put(name, eval(args.get(2), env));
		return null;
	}

	private static Proc lambda(List<?> args, Environment env) {
		if (args.size() != 3) {
			throw new EvalException(Errors.INVALID_NUMBER_OF_ARGUMENTS,
					3, 3, args.size()-1);
		}
		if (!(args.get(1) instanceof List || args.get(1) instanceof Symbol)) {
			throw new EvalException(Errors.EXPECTED_LIST_OF_ARGUMENTS,
					args.get(1));
		}

		Proc result = null;
		Object body = args.get(2);

		if (args.get(1) instanceof List) {
			// If formal args appear as list, bind each variable individually.
			List<?> formalArgs = (List<?>) args.get(1);
			List<String> formalArgNames = new LinkedList<String>();
			for (Object o : formalArgs) {
				if (!(o instanceof Symbol)) {
					throw new EvalException(Errors.EXPECTED_LIST_OF_ARGUMENTS,
							o);
				}
				formalArgNames.add(((Symbol)o).toString());
			}
			result = new Proc(formalArgNames, env, body);

		} else if (args.get(1) instanceof Symbol) {
			 // If formal arg is a symbol, then all arguments are bound to a
			 // single variable when invoked.
			result = new Proc(Collections.singletonList(args.get(1).toString()),
					env, body, true);
		}

		return result;
	}

	private static Object proc(List<?> list, Environment env) {
		// Head element is proc object.  Tail is list of actual arguments
		List<Object> evaldList = new LinkedList<Object>();
		for (Object elem : list) {
			evaldList.add(eval(elem, env));
		}
		int numOfActualArgs = evaldList.size() - 1;

		if (!(evaldList.get(0) instanceof Proc)) {
			throw new EvalException(Errors.PROC_EXPECTED);
		}

		Proc proc = (Proc) evaldList.get(0);
		List<String> formalArgs = proc.formalArguments;

		// Bind arguments
		Environment procEnv = new Environment(proc.environment);
		if (proc.boundAllArgsAsList) {
			// Turn all actual arguments into list, pass as sole parameter
			if (formalArgs.size() != 1) {
				throw new EvalException(Errors.INTERNAL_ERROR, "when binding " +
						"all arguments to proc as list, only one formal " +
						"argument allowed");
			}
			List<Object> mergedActualArguments = new LinkedList<Object>();
			for (int i=1; i<evaldList.size(); ++i) {
				mergedActualArguments.add(evaldList.get(i));
			}
			procEnv.put(formalArgs.get(0), mergedActualArguments);
		} else {
			// Bind each argument to different formal argument
			if (formalArgs.size() != numOfActualArgs) {
				throw new EvalException(Errors.INVALID_NUMBER_OF_ARGUMENTS,
					formalArgs.size(), formalArgs.size(), numOfActualArgs);
			}

			for (int i=0; i<formalArgs.size(); ++i) {
				procEnv.put(formalArgs.get(i), evaldList.get(i+1));
			}
		}

		return eval(proc.body, procEnv);  // exec in nested environment
	}
}


class Lexer {
	private static String DOT = ".";
	private static String TRUE = "#t";
	private static String FALSE = "#f";

	private List<String> tokens;
	private int index;

	public Lexer(String program) {
		tokens = tokenize(program);
		index = 0;
	}

	static List<String> tokenize(String program) {
		program = program.replaceAll("\\(", " ( ")
				         .replaceAll("\\)", " ) ");
		List<String> result = new LinkedList<String>();
		StringTokenizer tokenizer = new StringTokenizer(program);
		while (tokenizer.hasMoreTokens()) {
			result.add(tokenizer.nextToken());
		}
		return result;
	}

	public int size() {
		return tokens.size();
	}

	public boolean hasMoreTokens() {
		return (index < tokens.size());
	}

	public Object peekToken() throws ParseException {
		if (!hasMoreTokens()) {
			throw new ParseException(Errors.PREMATURE_END_OF_PROGRAM);
		}
		String input = tokens.get(index);
		if (("(".equals(input)) || ")".equals(input))
		 {
			return input;  // as is
		}
		return toAtom(input);
	}

	public Object consumeToken() throws ParseException {
		if (!hasMoreTokens()) {
			throw new ParseException(Errors.PREMATURE_END_OF_PROGRAM);
		}
		String input = tokens.get(index++);
		if (("(".equals(input)) || ")".equals(input)) {
			return input;  // as is
		}
		return toAtom(input);
	}

	public Object toAtom(String input) {
		try {
			return Double.parseDouble(input);
		} catch (NumberFormatException ignore) { }

		if (TRUE.equals(input)) {
			return Boolean.TRUE;
		} else if (FALSE.equals(input)) {
			return Boolean.FALSE;
		} else if (input.startsWith(DOT)) {
			return new Dot(input.substring(1, input.length()));
		} else {
			SpecialForm sf = SpecialForm.getByLiteralVal(input);
			return (sf != null ? sf : new Symbol(input));
		}
	}

	public void expect(Object token) throws ParseException {
		Object actual = consumeToken();
		if (!actual.equals(token)) {
			throw new ParseException(Errors.UNEXPECTED_TOKEN, token, actual);
		}
	}

	public void print(PrintStream out) {
		String separator = "";
		for (String token : tokens) {
			out.print(separator + " \"" + token + "\"");
			separator = ",";
		}
	}
}


class Builtins {

	private static String ENV = Builtins.class.getName();

	public static Environment getGlobalEnvironment() {
		Environment env= new Environment(null);
		env.put("+", new Proc(strList("a", "b"), env, read("(.plus "+ ENV +" a b)")));
		env.put("-", new Proc(strList("a", "b"), env, read("(.minus "+ ENV +" a b)")));
		env.put("*", new Proc(strList("a", "b"), env, read("(.mul "+ ENV +" a b)")));
		env.put("/", new Proc(strList("a", "b"), env, read("(.div "+ ENV +" a b)")));
		env.put("%", new Proc(strList("a", "b"), env, read("(.mod "+ ENV +" a b)")));
		env.put("<", new Proc(strList("a", "b"), env, read("(.lt "+ ENV +" a b)")));
		env.put(">", new Proc(strList("a", "b"), env, read("(.gt "+ ENV +" a b)")));
		env.put("<=", new Proc(strList("a", "b"), env, read("(.lte "+ ENV +" a b)")));
		env.put(">=", new Proc(strList("a", "b"), env, read("(.gte "+ ENV +" a b)")));
		env.put("=", new Proc(strList("a", "b"), env, read("(.equal1 "+ ENV +" a b)")));
		env.put("equal?", new Proc(strList("a", "b"), env, read("(.equal2 "+ ENV +" a b)")));
		env.put("eq?", new Proc(strList("a", "b"), env, read("(.equal3 "+ ENV +" a b)")));
		env.put("not", new Proc(strList("a"), env, read("(.not "+ ENV +" a)")));
		env.put("length", new Proc(strList("a"), env, read("(.length "+ ENV +" a)")));
		env.put("cons", new Proc(strList("a", "b"), env, read("(.cons "+ ENV +" a b)")));
		env.put("car", new Proc(strList("a"), env, read("(.car "+ ENV +" a)")));
		env.put("cdr", new Proc(strList("a"), env, read("(.cdr "+ ENV +" a)")));
		env.put("append", new Proc(strList("a", "b"), env, read("(.append "+ ENV +" a b)")));
		env.put("list", new Proc(strList("a"), env, read("(.list "+ ENV +" a)"), true));
		env.put("list?", new Proc(strList("a", "b"), env, read("(.list2 "+ ENV +" a b)")));
		env.put("null?", new Proc(strList("a"), env, read("(.null1 "+ ENV +" a)")));
		env.put("symbol?", new Proc(strList("a"), env, read("(.symbol "+ ENV +" a)")));
		env.put("display", new Proc(strList("a"), env, read("(.display " + ENV + " a)"), true));
		return env;
	}

    private static List<String> strList(String... objects) {
    	return Arrays.asList(objects);
    }

	static Double plus(Double a, Double b) { return a + b; }
    static Double minus(Double a, Double b) { return a - b; }
    static Double mul(Double a, Double b) { return a * b; }
    static Double div(Double a, Double b) { return a / b; }
    static Double mod(Double a, Double b) { return a % b; }
    static Boolean lt(Double a, Double b) { return a < b; }
    static Boolean gt(Double a, Double b) { return a > b; }
    static Boolean lte(Double a, Double b) { return a <= b; }
    static Boolean gte(Double a, Double b) { return a >= b; }
    static Boolean equal1(Double a, Double b) { return equalObject(a,b); }
    static Boolean equal2(Double a, Double b) { return equalObject(a, b); }
    static Double length(LinkedList<?> a) { return new Double(a.size()); }
    static Boolean equal3(Double a, Double b) { return a == b; }
    static Object list(LinkedList<?> a) { return a; }
    static Boolean list2(Object a) { return (a instanceof List); }
    static Boolean null1(LinkedList<?> a) { return (a == null || a.size() == 0); }
    static Boolean null1(Double a) { return false; }
    static Boolean symbol(Object a) { return (a instanceof Symbol); }
    

    static Boolean equalObject(Object a, Object b) {
    	if (a == null && b == null) {
			return true;
		} else if (a != null && b != null && a.equals(b)) {
			return true;
		} else {
			return false;
		}
    }

    static Boolean not(Boolean arg) {
    	return (arg != null ? !arg.booleanValue() : false);
    }

    static LinkedList<Object> cons(LinkedList<?> a, LinkedList<?> b) {
    	LinkedList<Object> result = new LinkedList<Object>();
    	result.add(a);
    	if (b != null) {
			result.addAll(b);
		}
    	return result;
    }

    static LinkedList<Object> cons(Double a, LinkedList<?> b) {
    	LinkedList<Object> result = new LinkedList<Object>();
    	result.add(a);
    	if (b != null) {
			result.addAll(b);
		}
    	return result;
    }

    static Object car(LinkedList<?> a) {
    	return (a != null && a.size() > 0 ? a.getFirst(): null);
    }

    static Object cdr(LinkedList<Object> a) {
    	if (a != null && a.size() > 1) {
    		LinkedList<Object> result = new LinkedList<Object>(a);
    		result.removeFirst();
    		return result;
    	} else {
    		return new LinkedList<Object>();
    	}
    }

    static LinkedList<Object> append(LinkedList<?> a, LinkedList<?> b) {
    	LinkedList<Object> result = new LinkedList<Object>();
    	if (a != null) {
			result.addAll(a);
		}
    	if (b != null) {
			result.addAll(b);
		}
    	return result;
    }

}

enum SpecialForm {

	QUOTE("quote"), IF("if"), SET_("set!"), DEFINE("define"), LAMBDA("lambda"),
	BEGIN("begin"), PROC("proc"), DOT(".");

	final String literalVal;

	SpecialForm(final String literalVal) {
		this.literalVal = literalVal;
	}

	public static SpecialForm getByLiteralVal(final String literalVal) {
		for (SpecialForm sf : values()) {
			if (sf.literalVal.equals(literalVal)) {
				return sf;
			}
		}
		return null;
	}
}


class Proc {

	final List<String> formalArguments;
	final Environment environment;
	final Object body;

	boolean boundAllArgsAsList = false;

	public Proc(final List<String> formalArguments,
			final Environment environment, final Object body,
			final boolean boundAllArgsAsList) {
		this(formalArguments, environment, body);
		this.boundAllArgsAsList = boundAllArgsAsList;
	}

	public Proc(final List<String> formalArguments,
			final Environment environment, final Object body) {
		this.formalArguments = formalArguments;
		this.environment = environment;
		this.body = body;
	}
}

class Symbol {

	final String symbol;

	public Symbol(final String symbol) {
		this.symbol = symbol;
	}

	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((symbol == null) ? 0 : symbol.hashCode());
		return result;
	}

	public boolean equals(final Object obj) {
		if (this == obj) { return true; }
		if (obj == null) { return false; }
		if (getClass() != obj.getClass()) { return false; }
		Symbol other = (Symbol) obj;
		if (symbol == null) {
			if (other.symbol != null) { return false; }
		} else if (!symbol.equals(other.symbol)) {
			return false;
		}
		return true;
	}
	
	@Override
	public String toString() {
		return symbol;
	}
}

class Dot {

	final String methodName;

	public Dot(final String methodName) {
		this.methodName = methodName;
	}
}
