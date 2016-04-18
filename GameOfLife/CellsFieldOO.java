/**
 * @author Kmikzy - FunkyWork
 * */
public class CellsFieldOO {
	public Cell[][] field;
	private int width;
	private int height;
	
	public CellsFieldOO(int w, int h){
		this.width = w;
		this.height = h;
		this.field = new Cell[this.width][this.height];
	}
	
	public void init(){
		for (int i = 1; i < this.width-1; i++) {
			this.field[i][0] = new UpperMiddleCell(i, 0);
			this.field[i][this.height-1] = new LowerMiddleCell(i, this.height-1);
			for (int j = 1; j < this.height-1; j++) {
				this.field[i][j] = new MiddleCell(i, j);
			}
		}
		for (int j = 1; j < this.height-1; j++) {
			this.field[0][j] = new MiddleLeftCell(0, j);
			this.field[this.width-1][j] = new MiddleRightCell(this.width-1, j);
		}
		this.field[0][0] = new UpperLeftCell(0, 0);
		this.field[this.width-1][0] = new UpperRightCell(this.width-1, 0);
		this.field[0][this.height-1] = new LowerLeftCell(0, this.height-1);
		this.field[this.width-1][this.height-1] = new LowerRightCell(this.width-1, this.height-1);
		
		this.field[25][25].setLive(true);
		this.field[25][26].setLive(true);
		this.field[25][27].setLive(true);
		this.field[24][26].setLive(true);
		this.field[25][26].setLive(true);
		this.field[26][26].setLive(true);
		
		this.field[30][30].setLive(true);
		this.field[30][31].setLive(true);
		this.field[30][32].setLive(true);
		this.field[29][31].setLive(true);
		this.field[30][31].setLive(true);
		this.field[31][31].setLive(true);
		
		this.field[35][35].setLive(true);
		this.field[35][36].setLive(true);
		this.field[35][37].setLive(true);
		this.field[34][36].setLive(true);
		this.field[35][36].setLive(true);
		this.field[36][36].setLive(true);
	}
	
	public void update() {
		CellsFieldOO nextField = new CellsFieldOO(this.width, this.height);
		for (int i = 0; i < this.width; i++) {
			for (int j = 0; j < this.height; j++) {
				nextField.field[i][j] = this.field[i][j].clone();
				nextField.field[i][j].setLive(this.field[i][j].nextValue());
			}
		}
		this.field = nextField.field;
	}
	
	public String toString(){
		String s = "\n";
		for (int j = 0; j < this.height; j++) {
			for (int i = 0; i < this.width; i++)
				s += this.field[i][j];
			s += "\n";
		}
		return s;
	}
	
	private abstract class Cell{
		private boolean alive;
		private int posX;
		private int posY;
		public boolean isAlive() { return alive; }
		public void setLive(boolean b) { alive = b; }
		public String toString() { return alive ? "0" : "."; }
		public abstract boolean nextValue();
		public abstract Cell clone();
	}
	
	private class UpperLeftCell extends Cell{
		public UpperLeftCell(int x, int y) {
			super.posX = x;
			super.posY = y;
		}
		@Override
		public boolean nextValue() {
			int n = 0;
			if (CellsFieldOO.this.field[super.posX+1][super.posY].isAlive()) n++;
			if (CellsFieldOO.this.field[super.posX][super.posY+1].isAlive()) n++;
			if (CellsFieldOO.this.field[super.posX+1][super.posY+1].isAlive()) n++;
			if (n == 2) return super.isAlive();
			return n == 3;
		}
		@Override
		public Cell clone(){
			UpperLeftCell c = new UpperLeftCell(super.posX, super.posY);
			c.setLive(super.isAlive());
			return c;
		}
	}
	private class UpperMiddleCell extends Cell{
		public UpperMiddleCell(int x, int y) {
			super.posX = x;
			super.posY = y;
		}
		@Override
		public boolean nextValue() {
			int n = 0;
			if (CellsFieldOO.this.field[super.posX-1][super.posY].isAlive()) n++;
			if (CellsFieldOO.this.field[super.posX+1][super.posY].isAlive()) n++;
			if (CellsFieldOO.this.field[super.posX-1][super.posY+1].isAlive()) n++;
			if (CellsFieldOO.this.field[super.posX][super.posY+1].isAlive()) n++;
			if (CellsFieldOO.this.field[super.posX+1][super.posY+1].isAlive()) n++;
			if (n == 2) return super.isAlive();
			return n == 3;
		}
		@Override
		public Cell clone(){
			UpperMiddleCell c = new UpperMiddleCell(super.posX, super.posY);
			c.setLive(super.isAlive());
			return c;
		}
	}
	private class UpperRightCell extends Cell{
		public UpperRightCell(int x, int y) {
			super.posX = x;
			super.posY = y;
		}
		@Override
		public boolean nextValue() {
			int n = 0;
			if (CellsFieldOO.this.field[super.posX-1][super.posY].isAlive()) n++;
			if (CellsFieldOO.this.field[super.posX-1][super.posY+1].isAlive()) n++;
			if (CellsFieldOO.this.field[super.posX][super.posY+1].isAlive()) n++;
			if (n == 2) return super.isAlive();
			return n == 3;
		}
		@Override
		public Cell clone(){
			UpperRightCell c = new UpperRightCell(super.posX, super.posY);
			c.setLive(super.isAlive());
			return c;
		}
	}
	private class MiddleLeftCell extends Cell{
		public MiddleLeftCell(int x, int y) {
			super.posX = x;
			super.posY = y;
		}
		@Override
		public boolean nextValue() {
			int n = 0;
			if (CellsFieldOO.this.field[super.posX][super.posY-1].isAlive()) n++;
			if (CellsFieldOO.this.field[super.posX+1][super.posY-1].isAlive()) n++;
			if (CellsFieldOO.this.field[super.posX+1][super.posY].isAlive()) n++;
			if (CellsFieldOO.this.field[super.posX][super.posY+1].isAlive()) n++;
			if (CellsFieldOO.this.field[super.posX+1][super.posY+1].isAlive()) n++;
			if (n == 2) return super.isAlive();
			return n == 3;
		}
		@Override
		public Cell clone(){
			MiddleLeftCell c = new MiddleLeftCell(super.posX, super.posY);
			c.setLive(super.isAlive());
			return c;
		}
	}
	private class MiddleCell extends Cell{
		public MiddleCell(int x, int y) {
			super.posX = x;
			super.posY = y;
		}
		@Override
		public boolean nextValue() {
			int n = 0;
			if (CellsFieldOO.this.field[super.posX-1][super.posY-1].isAlive()) n++;
			if (CellsFieldOO.this.field[super.posX][super.posY-1].isAlive()) n++;
			if (CellsFieldOO.this.field[super.posX+1][super.posY-1].isAlive()) n++;
			if (CellsFieldOO.this.field[super.posX-1][super.posY].isAlive()) n++;
			if (CellsFieldOO.this.field[super.posX+1][super.posY].isAlive()) n++;
			if (CellsFieldOO.this.field[super.posX-1][super.posY+1].isAlive()) n++;
			if (CellsFieldOO.this.field[super.posX][super.posY+1].isAlive()) n++;
			if (CellsFieldOO.this.field[super.posX+1][super.posY+1].isAlive()) n++;
			if (n == 2) return super.isAlive();
			return n == 3;
		}
		@Override
		public Cell clone(){
			MiddleCell c = new MiddleCell(super.posX, super.posY);
			c.setLive(super.isAlive());
			return c;
		}
	}
	private class MiddleRightCell extends Cell{
		public MiddleRightCell(int x, int y) {
			super.posX = x;
			super.posY = y;
		}
		@Override
		public boolean nextValue() {
			int n = 0;
			if (CellsFieldOO.this.field[super.posX-1][super.posY-1].isAlive()) n++;
			if (CellsFieldOO.this.field[super.posX][super.posY-1].isAlive()) n++;
			if (CellsFieldOO.this.field[super.posX-1][super.posY].isAlive()) n++;
			if (CellsFieldOO.this.field[super.posX-1][super.posY+1].isAlive()) n++;
			if (CellsFieldOO.this.field[super.posX][super.posY+1].isAlive()) n++;
			if (n == 2) return super.isAlive();
			return n == 3;
		}
		@Override
		public Cell clone(){
			MiddleRightCell c = new MiddleRightCell(super.posX, super.posY);
			c.setLive(super.isAlive());
			return c;
		}
	}
	private class LowerLeftCell extends Cell{
		public LowerLeftCell(int x, int y) {
			super.posX = x;
			super.posY = y;
		}
		@Override
		public boolean nextValue() {
			int n = 0;
			if (CellsFieldOO.this.field[super.posX][super.posY-1].isAlive()) n++;
			if (CellsFieldOO.this.field[super.posX+1][super.posY-1].isAlive()) n++;
			if (CellsFieldOO.this.field[super.posX+1][super.posY].isAlive()) n++;
			if (n == 2) return super.isAlive();
			return n == 3;
		}
		@Override
		public Cell clone(){
			LowerLeftCell c = new LowerLeftCell(super.posX, super.posY);
			c.setLive(super.isAlive());
			return c;
		}
	}
	private class LowerMiddleCell extends Cell{
		public LowerMiddleCell(int x, int y) {
			super.posX = x;
			super.posY = y;
		}
		@Override
		public boolean nextValue() {
			int n = 0;
			if (CellsFieldOO.this.field[super.posX-1][super.posY-1].isAlive()) n++;
			if (CellsFieldOO.this.field[super.posX][super.posY-1].isAlive()) n++;
			if (CellsFieldOO.this.field[super.posX+1][super.posY-1].isAlive()) n++;
			if (CellsFieldOO.this.field[super.posX-1][super.posY].isAlive()) n++;
			if (CellsFieldOO.this.field[super.posX+1][super.posY].isAlive()) n++;
			if (n == 2) return super.isAlive();
			return n == 3;
		}
		@Override
		public Cell clone(){
			LowerMiddleCell c = new LowerMiddleCell(super.posX, super.posY);
			c.setLive(super.isAlive());
			return c;
		}
	}
	private class LowerRightCell extends Cell{
		public LowerRightCell(int x, int y) {
			super.posX = x;
			super.posY = y;
		}
		@Override
		public boolean nextValue() {
			int n = 0;
			if (CellsFieldOO.this.field[super.posX-1][super.posY-1].isAlive()) n++;
			if (CellsFieldOO.this.field[super.posX][super.posY-1].isAlive()) n++;
			if (CellsFieldOO.this.field[super.posX-1][super.posY].isAlive()) n++;
			if (n == 2) return super.isAlive();
			return n == 3;
		}
		@Override
		public Cell clone(){
			LowerRightCell c = new LowerRightCell(super.posX, super.posY);
			c.setLive(super.isAlive());
			return c;
		}
	}
}
