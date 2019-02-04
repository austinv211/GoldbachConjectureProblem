
# coding: utf-8

# In[1]:


import math as Math

'''
Class for implementing Goldbach
other conjecture
'''
class Goldbach:
    
    '''
    method isPrime to check if a number is 
    prime or non-prime
    The method accepts an argument n which is a 
    number which we need to check if it is prime or not.
    This method returns a boolean value
    i.e. either True or False.
    True: if number is prime
    False: if number is non-prime
    '''
    def isPrime(self,n):
        i = 0
        '''
        a number will only be fully divided
        by a number which is either half of that
        number or is less than half of that number
        The division will return a double value which
        '''
        m = n/2
        '''
        a boolean variable flag which is the value returned by 
        the isPrime method
        '''
        flag = True
        
        '''
        if the number which we passed is 0 or 1 then it will
        directly return False as 0 and 1 are neither prime
        or non-prime numbers
        '''
        if n == 0 or n == 1:
                return False
        else:
            '''
            for loop running from 2 to m
            in this I have written m+1 because 
            m+1 is excluded from the range so 
            the loop will run from 2 to m
            I have taken Math.floor value of m+1
            because of the division m is double
            '''
            for i in range(2,Math.floor(m+1)):
                '''
                As soon as if a number totally divides the number n,
                then n is non-prime.
                '''
                if n%i == 0:
                    flag = False
                    break
        #print(flag)
        return flag

    '''
    method to generate a list of non-prime
    odd numbers
    the method accepts an argument max i.e. 
    till which number we want to generate the list.
    The method returns a list of integers.
    '''
    def getOddNonPrimes(self,max):
        # An empty list which we will return after 
        # appending the odd non-prime numbers to it
        oddNonPrimes = []
        
        '''
        In this for loop since, smallest odd prime number
        is 9, so we have started the loop from 9 till max+1 
        which is excluded from the range, so the loop will
        run from 9 to max. 
        The step size we have defined is 2 so that it only checks
        for odd numbers.
        '''
        for i in range(9,max+1, 2):
            '''
            Calling the isPrime method to check for all i's
            if it is prime or not.
            If the number is prime then we will not add it in the list i.e
            isPrime will return True if the number is prime and we have added 'not'
            in the if condition so that condition will become False.
            If the number is not prime then isPrime will reutrn False, which will satisfy
            the if condition and then the number will be appended to the list.
            '''
            if not self.isPrime(i):
                oddNonPrimes.append(i)
        #print(oddNonPrimes)
        return oddNonPrimes

    '''
    Method to check is a number is a perfect square or not.
    This method is actually implementing the Goldbach conjecture
    logic.
    It checks for the prime 'p' so that k^2 = (g-p)/2
    This method return true if the non-prime number which we 
    passed is sum of a prime and twice a square i.e.
    g = p + 2*k^2
    '''
    def isPerfectSquare(self,g):
         
        '''
        To find all possible combination to find for which p and k
        goldbach's conjecture hold true we have started checking for
        p with a value 2 less than g as even if k = 1 then the number
        which is 1 less than p will not give the original number after
        adding with 2 i.e. if g = 9 and p = g-1 = 9-1 = 8
        then from g = p + 2*k^2 will result as
        9 = 8 + 2*1^2 i.e. 9 \= 10, whereas the correct expression for 9
        should be p = 9-2 = 7 and from the equation above
        9 = 7+2*1^2 i.e. 9 == 7+2 = 9
        '''
        p = g-2
        flag = False
        '''
        This while loop will check for all the p's till p becomes 0
        '''
        while p>0:
            '''
            Calling function isPrime to check if p is prime or not as
            the 1st condition in Goldbach's other conjecture is that
            every non-prime number i.e. g in this method must be sum
            of a prime i.e. p in this method and twice the square.
            '''
            if self.isPrime(p):
                '''
                from the euqation g = p + 2*k^2
                we have derived the equation 
                k^2 = (g-p)/2 i.e. sq is representing 
                k^2 in this method
                '''
                sq = (g-p)/2
                '''
                isSq is representing k in this method i.e.
                k = ((g-p)/2)^(1/2)
                '''
                isSq = Math.sqrt(sq)
                '''
                Condition to check if the number is a perfect suqare or not
                If the number is perfect square then the loop will stop and will
                start checking for other number.
                '''
                if isSq == Math.floor(isSq):
                    flag = True
                    break
            p = p-1
        #print(flag)
        return flag
    
    '''
    This method is for checking the numbers for which 
    Goldbach's other conjecture does not hold true
    '''
    def goldBachConjecture(self,oddNonPrimes):
        result = []
        '''
        This for loop will iterate over the list oddNonPrimes which 
        is passed as an argument to this method.
        '''
        for i in oddNonPrimes:
            '''
            This if condition will check if the number holds goldbach's other 
            conjecture or not. If not then the number will be added to the final
            result list which will contains the numbers which does not hold goldbach's
            othe conjecture.
            '''
            if not self.isPerfectSquare(i):
                result.append(i)
        '''
        This for loop will iterate over the final result list which will
        prime the numbers which does not hold Goldbach's other conjecture
        '''
        for i in result:
            print(i)

    '''
    Main method which calls different methods in a squence 
    so that we get the functionality of the class for which 
    it is written
    '''
    def main(self):
        max = 10000
        oddNonPrimes = self.getOddNonPrimes(max)
        self.goldBachConjecture(oddNonPrimes)


# In[2]:


'''
Making object of Goldbach class
'''
g = Goldbach()


# In[3]:


'''
timeit library to record execution
time of the program
'''
import timeit
# start time of the program
start = timeit.default_timer()
# calling main method of the class
g.main()
# stop time of the program
stop = timeit.default_timer()
# printing the total execution time of the program i.e. difference between stop and start time
print('Time: ', stop - start)

