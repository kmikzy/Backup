import java.util.Timer;

/**
 * @author Kmikzy - FunkyWork
 * */
public class GameOfLife {
	
	public static void main(String[] args) {
		long time;
		
		CellsFieldOObis f1 = new CellsFieldOObis(90, 90);
		f1.init();
		time = System.currentTimeMillis();
		for (int i = 0; i < 100000; i++)
			f1.update();
		time = System.currentTimeMillis() - time;
		System.out.println("Temps pour calculer CellsFieldOObis : "+time);
		
		CellsFieldOO f2 = new CellsFieldOO(90, 90);
		f2.init();
		time = System.currentTimeMillis();
		for (int i = 0; i < 100000; i++)
			f2.update();
		time = System.currentTimeMillis() - time;
		System.out.println("Temps pour calculer CellsFieldOO : "+time);	
	}
	
	
}
