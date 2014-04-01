public class MangleNames
{
	public MangleNames()
	{
	}

	public void foo(int x)
	{
		x = x + 1;
	}

	public void foo(int x, Object y)
	{
		x = x + 1;
	}

	public void foo(int x, int y)
	{
		x = x + y;
	}
}
