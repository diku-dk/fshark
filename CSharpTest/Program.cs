using System;
using Matrix;
namespace CSharpTest
{
    internal class Program
    {
        public static void Main(string[] args)
        {
            var n = 5;
            var m = 7;
            var p = 6;
            
            var matrix = new Matrix.Matrix(args);

            var A = matrix.replicateRows(n, m);
            var B = matrix.replicateRows(m, p);
            var AB = matrix.multiply(A, B);

            
            for (int i = 0; i < n; i++)
            {
                for (int j = 0; j < p; j++)
                {
                    Console.Write("{0}\t", AB.Item1[(i*p)+j]);
                }
                Console.Write("\n");
            }
            Environment.Exit(0);
        }
    }
}