public class InstanceOfArray
{
	public InstanceOfArray()
	{
	}

	public static int test()
	{
		Object [] a = new Object[6];
		boolean b = a instanceof Object[];
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
