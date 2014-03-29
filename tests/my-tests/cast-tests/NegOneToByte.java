public class NegOneToByte
{
	public NegOneToByte()
	{
	}

	public static int test()
	{
		byte ret = (byte) -1;
		if ((int)ret == -1)
		{
			return 1;
		}
		else 
		{

			return -1;
		}
	}
}
