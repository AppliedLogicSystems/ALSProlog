import java.util.Arrays;

public class test {
    public static void hello() {
        System.out.println("Hello World");
    }
    public static int return_int() {
    	return 25;
    }
    public static double return_double() {
    	return 3.1415;
    }
    public static String return_string() {
    	return "test string";
    }
    public static void int_arg(int i) {
         System.out.println(i);
    }
    public static void double_arg(double d) {
         System.out.println(d);
    }
    public static void string_arg(String s) {
         System.out.println(s);
    }
    public static void all_args(int i, double d, String s) {
         System.out.println(i);
         System.out.println(d);
         System.out.println(s);
    }

    public static Object[] return_list() {
		Object l[] = new Object[3];
		l[0] = new Integer(5);
		l[1] = new Double(3.14);
		l[2] = "string";
    	return l;
    }
    public static Object[] return_sublist() {
		Object sl[] = new Object[3];
		sl[0] = new Integer(5);
		sl[1] = new Double(3.14);
		sl[2] = "string";
		Object l[] = new Object[4];
		l[0] = new Integer(5);
		l[1] = new Double(3.14);
		l[2] = sl;
		l[3] = "string";
    	return l;
    }

    public static void list_arg(Object l[]) {
    	System.out.println(l[0]);
    	System.out.println(l[1]);
    	System.out.println(l[2]);
    }
    
    public static void divzero() {
    	int x = 3;
    	int y = 0;
    	int z = x/y;
    }
    
    public static Object[] null_list() {
    	Object l[] = new Object[3];
    	return l;
    }
}
