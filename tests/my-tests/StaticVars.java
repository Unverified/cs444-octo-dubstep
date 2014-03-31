public class StaticVars
{
	public static int x1 = 6;
	public static int x2;
	public int x3;
	public static int x4 = 7;

	public StaticVars()
	{
	}
	public static int test()
	{
		if ((x1 == 6) && (x4 == 7))
		{
			return 1;
		}
		else
		{
			return -1;
		}
	}
}
