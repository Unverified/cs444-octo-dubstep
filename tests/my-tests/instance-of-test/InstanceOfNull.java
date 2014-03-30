public class InstanceOfNull
{
	public InstanceOfNull()
	{
	}

	public static int test()
	{
		InstanceOfNull a = null;
		boolean b = a instanceof InstanceOfNull;
		if (b == true)
		{
			return -1;
		}
		else
		{
			return 1;
		}
	}
}
