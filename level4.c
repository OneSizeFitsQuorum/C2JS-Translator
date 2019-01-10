#include "stdio.h"
#include "stdlib.h" 

int quickSort(int s[],int l,int r)
{
    if(l < r)
    {
        int i=l;
		int j=r;
		int x=s[l];
        while(i<j)
        {
            while(i<j && s[j]>=x)
			{
                j--;
			}
            if(i<j)
			{
                s[i++]=s[j];
			}

            while(i<j && s[i]<=x)
			{
                i++;
			}
            if(i<j)
			{
                s[j--]=s[i];
			}

        }

        s[i]=x;
        quickSort(s,l,i-1);
        quickSort(s,i+1,r);
    }
    return 0;
}

int main()
{ 
    int a[] = {1,8,44,77,35,65,78,12,25,49,20,15,45};
    int length = 13;

    for(int i=0;i<length;i++)
    {
        printf("%3d",a[i]);
    }

    printf("\n");
    quickSort(a,0,length-1);

    for(int i=0;i<length;i++)
    {
        printf("%3d",a[i]);
    }
    printf("\n");
    return 0;
}
