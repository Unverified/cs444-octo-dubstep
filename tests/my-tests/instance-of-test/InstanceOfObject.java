public class InstanceOfObject
{
	public InstanceOfObject()
	{
	}


	public static int test()
	{
		boolean b = "Hello" instanceof Object;
		if (b == true)
		{
			return 1;
		}
		else	
		{
			return -1;
		}
	}
}
