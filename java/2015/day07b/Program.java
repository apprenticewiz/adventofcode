import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.regex.*;

class Operation {
    public enum Operator {
        ASSIGN,
        NOT,
        AND,
        OR,
        LSHIFT,
        RSHIFT,
    }

    public Operator operator;
    public String source1;
    public String source2;
    public int amount;

    public Operation(Operator op, String src1) {
        operator = op;
        source1 = src1;
    }

    public Operation(Operator op, String src1, String src2) {
        operator = op;
        source1 = src1;
        source2 = src2;
    }

    public Operation(Operator op, String src1, int amt) {
        operator = op;
        source1 = src1;
        amount = amt;
    }
}

public class Program {
    public static void usage() {
        System.err.println("usage: java -jar Program.jar <input file>");
        System.exit(1);
    }

    public static int process(String filename) {
        HashMap<String, Operation> operations = new HashMap<String, Operation>();
        Pattern p1 = Pattern.compile("^(\\d+|\\w+) -> (\\w+)$");
        Pattern p2 = Pattern.compile("NOT (\\d+|\\w+) -> (\\w+)");
        Pattern p3 = Pattern.compile("(\\d+|\\w+) (AND|OR) (\\d+|\\w+) -> (\\w+)");
        Pattern p4 = Pattern.compile("(\\d+|\\w+) (LSHIFT|RSHIFT) (\\d+) -> (\\w+)");
        try ( BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line;
            while ( (line = reader.readLine()) != null ) {
                Matcher m1 = p1.matcher(line);
                if ( m1.matches() ) {
                    String src = m1.group(1);
                    String dest = m1.group(2);
                    operations.put(dest, new Operation(Operation.Operator.ASSIGN, src));
                }
                Matcher m2 = p2.matcher(line);
                if ( m2.matches() ) {
                    String src = m2.group(1);
                    String dest = m2.group(2);
                    operations.put(dest, new Operation(Operation.Operator.NOT, src));
                }
                Matcher m3 = p3.matcher(line);
                if ( m3.matches() ) {
                    String src1 = m3.group(1);
                    String opStr = m3.group(2);
                    String src2 = m3.group(3);
                    String dest = m3.group(4);
                    if ( opStr.equals("AND") ) {
                        operations.put(dest, new Operation(Operation.Operator.AND, src1, src2));
                    } else {
                        operations.put(dest, new Operation(Operation.Operator.OR, src1, src2));
                    }
                }
                Matcher m4 = p4.matcher(line);
                if ( m4.matches() ) {
                    String src1 = m4.group(1);
                    String opStr = m4.group(2);
                    int amt = Integer.parseInt(m4.group(3));
                    String dest = m4.group(4);
                    if ( opStr.equals("LSHIFT") ) {
                        operations.put(dest, new Operation(Operation.Operator.LSHIFT, src1, amt));
                    } else {
                        operations.put(dest, new Operation(Operation.Operator.RSHIFT, src1, amt));
                    }
                }
            }
        } catch ( IOException e ) {
            System.err.println("Error reading file: " + e.getMessage());
            System.exit(1);
        }
        int a = eval(operations, new HashMap<String, Integer>(), "a");
        Operation op = new Operation(Operation.Operator.ASSIGN, Integer.toString(a));
        operations.put("b", op);
        return eval(operations, new HashMap<String, Integer>(), "a");
    }

    public static int eval(HashMap<String, Operation> ops, HashMap<String, Integer> cache, String expr) {
        try {
            int r = Integer.parseInt(expr);
            return r;
        } catch ( NumberFormatException e) {}
        if ( cache.containsKey(expr) ) {
            return cache.get(expr);
        } else {
            int a, b, amt, r;
            Operation op = ops.get(expr);
            switch ( op.operator ) {
                case Operation.Operator.ASSIGN:
                    a = eval(ops, cache, op.source1);
                    r = a;
                    break;
                case Operation.Operator.NOT:
                    a = eval(ops, cache, op.source1);
                    r = ~a;
                    break;
                case Operation.Operator.AND:
                    a = eval(ops, cache, op.source1);
                    b = eval(ops, cache, op.source2);
                    r = a & b;
                    break;
                case Operation.Operator.OR:
                    a = eval(ops, cache, op.source1);
                    b = eval(ops, cache, op.source2);
                    r = a | b;
                    break;
                case Operation.Operator.LSHIFT:
                    a = eval(ops, cache, op.source1);
                    amt = op.amount;
                    r = a << amt;
                    break;
                case Operation.Operator.RSHIFT:
                    a = eval(ops, cache, op.source1);
                    amt = op.amount;
                    r = a >> amt;
                    break;
                default:
                    r = 0;
            }
            int masked = r & 0xffff;
            cache.put(expr, masked);
            return masked;
        }
    }

    public static final void main(String[] args) {
        if ( args.length < 1 ) {
            usage();
        }
        String filename = args[0];
        int result = process(filename);
        System.out.println("result = " + result);
    }
}
