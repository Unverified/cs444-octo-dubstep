public class CastTest
{
	public CastTest()
	{
	}
	public void foo()
	{
		int z = -1;
		int w = 32768;
		char x1 =(char) z;
		int x2 = (int) z;
		short x3 = (short) z;
		byte x4 = (byte) z;
		
		char y1 = (char) w;
		short y2 = (short) w;
		int y3 = (short) w;
		byte y4 = (byte) w;

	}
	
	public static int test()
	{

		CastTest c = new CastTest();
		return 9;
	}
}
