import java.util.Scanner;

/**
 * @author Kmikzy - FunkyWork
 * */
public class InterpreteurBrainfuck {

	private static Scanner scanner = new Scanner(System.in);
	
	private static char[] byteTab;		//tableau d'octets
	private static int posByteTab;
	private static char[] codeTab;		//code a executer
	private static int posCodeTab;	
	
	public static void main(String[] args) {
		String code = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.";
		byteTab = new char[10];
		posByteTab = 0;
		codeTab = code.toCharArray();
		posCodeTab = 0;
		interpret();
	}
	
	private static void doubleTabSize() {
		char[] n = new char[byteTab.length*2];
		for (int i = 0; i < byteTab.length; i++) n[i] = byteTab[i];
		byteTab = n;
	}
	
	/**
	 * Methodes pour pouvoir simuler un octet
	 * */
	private static void inc() {
		if (byteTab[posByteTab] >= 255) byteTab[posByteTab] = 0;
		else byteTab[posByteTab]++;
	}
	
	private static void dec() {
		if (byteTab[posByteTab] <= 0) byteTab[posByteTab] = 255;
		else byteTab[posByteTab]--;
	}
	
	private static void right() {
		if (posByteTab >= byteTab.length) doubleTabSize();
		posByteTab++;
	}
	
	private static void left() {
		posByteTab--;
	}
	
	private static void read() {
		byteTab[posByteTab] = scanner.nextLine().charAt(0);
	}
	
	private static void startLoop() {
		posCodeTab++;
		if (byteTab[posByteTab] == 0) {
			int n = 1;
			while (posCodeTab < codeTab.length && n != 0) {
				if (codeTab[posCodeTab] == '[') n++;
				else if (codeTab[posCodeTab] == ']') n--;
				posCodeTab++;
			}
		}
	}
	
	private static void endLoop() {
		if (byteTab[posByteTab] == 0) posCodeTab++;
		else {
			posCodeTab--;
			int n = 1;
			while (posCodeTab >= 0 && n != 0) {
				if (codeTab[posCodeTab] == '[') n--;
				else if (codeTab[posCodeTab] == ']') n++;
				posCodeTab--;
			}
			posCodeTab+=2;
		}
	}
	
	private static void interpret() {
		while (posCodeTab < codeTab.length) {
			switch (codeTab[posCodeTab]) {
				case '>' : right(); posCodeTab++; break;
				case '<' : left(); posCodeTab++; break;
				case '+' : inc(); posCodeTab++; break;
				case '-' : dec(); posCodeTab++; break;
				case '.' : System.out.print(byteTab[posByteTab]); posCodeTab++; break;
				case ',' : read(); posCodeTab++; break;
				case '[' : startLoop(); break;
				case ']' : endLoop();
			}
		}
	}
}
