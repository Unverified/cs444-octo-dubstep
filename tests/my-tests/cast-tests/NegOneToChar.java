public class NegOneToChar
{
	public NegOneToChar()
	{
	}

	public static int test()
	{
		int x = -1;
		char ret = (char) x;
		if ((int)ret == -1)
		{
			return -1;
		}
		else
		{
			return 1;
		}

	}
}
