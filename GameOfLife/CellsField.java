/**
 * @author Kmikzy - FunkyWork
 * */
public class CellsField {
	public char[][] field;
	private int width;
	private int height;
	
	public CellsField(int w, int h){
		this.width = w;
		this.height = h;
		this.field = new char[this.width][this.height];
	}
	
	public void init(){
		for (int i = 0; i < this.width; i++)
			for (int j = 0; j < this.height; j++) {
				this.field[i][j] = 'D';
			}
		this.field[5][5] = 'A';
		this.field[5][6] = 'A';
		this.field[5][7] = 'A';
		this.field[4][6] = 'A';
		this.field[5][6] = 'A';
		this.field[6][6] = 'A';
	}
	
	public void update() {
		CellsField nextField = new CellsField(this.width, this.height);
		for (int i = 0; i < this.width; i++) {
			for (int j = 0; j < this.height; j++) {
				int n = this.nbNeighbors(i, j);
				if (n == 3) nextField.field[i][j] = 'A';
				else if (n == 2) nextField.field[i][j] = this.field[i][j];
				else nextField.field[i][j] = 'D';
			}
		}
		this.field = nextField.field;
	}
	
	private int nbNeighbors(int x, int y) {
		int n = 0;
		
		
		if (x-1 >= 0){
			if (x+1 < this.width) {
				if (y-1 >= 0) {
					if (y+1 < this.height) {
						/*-------
						 *|_|_|_|
						 *|_|x|_|
						 *|_|_|_|
						 *------- 
						 * */
						if ( this.field[x-1][y-1] == 'A') n++;
						if ( this.field[x][y-1] == 'A') n++;
						if ( this.field[x+1][y-1] == 'A') n++;
						
						if ( this.field[x-1][y] == 'A') n++;
						if ( this.field[x+1][y] == 'A') n++;
						
						if ( this.field[x-1][y+1] == 'A') n++;
						if ( this.field[x][y+1] == 'A') n++;
						if ( this.field[x+1][y+1] == 'A') n++;
						
						return n;
					}
					/*-------
					 *|_|_|_|
					 *|_|x|_|
					 *------- 
					 * */
					if ( this.field[x-1][y-1] == 'A') n++;
					if ( this.field[x][y-1] == 'A') n++;
					if ( this.field[x+1][y-1] == 'A') n++;
					
					if ( this.field[x-1][y] == 'A') n++;
					if ( this.field[x+1][y] == 'A') n++;
					
					return n;
				}
				/*-------
				 *|_|x|_|
				 *|_|_|_|
				 *------- 
				 * */
				if ( this.field[x-1][y] == 'A') n++;
				if ( this.field[x+1][y] == 'A') n++;
				
				if ( this.field[x-1][y+1] == 'A') n++;
				if ( this.field[x][y+1] == 'A') n++;
				if ( this.field[x+1][y+1] == 'A') n++;
				
				return n;
			}
			if (y-1 >= 0) {
				if (y+1 < this.height) {
					/*-----
					 *|_|_|
					 *|_|x|
					 *|_|_|
					 *----- 
					 * */
					if ( this.field[x-1][y-1] == 'A') n++;
					if ( this.field[x][y-1] == 'A') n++;
					
					if ( this.field[x-1][y] == 'A') n++;
					
					if ( this.field[x-1][y+1] == 'A') n++;
					if ( this.field[x][y+1] == 'A') n++;
					
					return n;
				}
				/*-----
				 *|_|_|
				 *|_|x|
				 *----- 
				 * */
				if ( this.field[x-1][y-1] == 'A') n++;
				if ( this.field[x][y-1] == 'A') n++;
				
				if ( this.field[x-1][y] == 'A') n++;
				
				return n;
			}
			/*-----
			 *|_|x|
			 *|_|_|
			 *----- 
			 * */
			if ( this.field[x-1][y] == 'A') n++;
			
			if ( this.field[x-1][y+1] == 'A') n++;
			if ( this.field[x][y+1] == 'A') n++;
			
			return n;
		}
		if (y-1 >= 0) {
			if (y+1 < this.height) {
				/*-----
				 *|_|_|
				 *|x|_|
				 *|_|_|
				 *----- 
				 * */
				if ( this.field[x][y-1] == 'A') n++;
				if ( this.field[x+1][y-1] == 'A') n++;
				
				if ( this.field[x+1][y] == 'A') n++;
				
				if ( this.field[x][y+1] == 'A') n++;
				if ( this.field[x+1][y+1] == 'A') n++;
				
				return n;
			}
			/*-----
			 *|_|_|
			 *|x|_|
			 *----- 
			 * */
			if ( this.field[x][y-1] == 'A') n++;
			if ( this.field[x+1][y-1] == 'A') n++;
			
			if ( this.field[x+1][y] == 'A') n++;
			
			return n;
		}
		/*-----
		 *|x|_|
		 *|_|_|
		 *----- 
		 * */
		if ( this.field[x+1][y] == 'A') n++;
		
		if ( this.field[x][y+1] == 'A') n++;
		if ( this.field[x+1][y+1] == 'A') n++;
		
		return n;
	}
	
	public String toString(){
		String s = "\n";
		for (int i = 0; i < this.width; i++) {
			for (int j = 0; j < this.height; j++)
				s += this.field[i][j];
			s += "\n";
		}
		return s;
	}
}
