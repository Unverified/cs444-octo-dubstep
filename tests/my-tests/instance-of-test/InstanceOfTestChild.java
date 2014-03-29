import InstanceOfTest;

public class InstanceOfTestChild extends InstanceOfTest
{
	public InstanceOfTestChild()
	{
	}

	public static int test()
	{
		boolean b = new InstanceOfTest() instanceof InstanceOfTestChild;
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
