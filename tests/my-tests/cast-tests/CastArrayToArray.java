public  class CastArrayToArray
{
	public CastArrayToArray()
	{
	}
	
	public static int test()
	{
		String[] a = new String[5];
		Object[] b = (Object[]) a;
		if (b == a)
		{
			return 1;
		}
		else 
		{
			return -1;
		}
	}
}
