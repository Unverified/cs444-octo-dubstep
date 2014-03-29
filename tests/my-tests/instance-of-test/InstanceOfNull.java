public class InstanceOfNull
{
	public InstanceOfNull()
	{
	}

	public static boolean foo()
	{
		InstanceOfNull a = null;
		return a instanceof Object;
	}
}
