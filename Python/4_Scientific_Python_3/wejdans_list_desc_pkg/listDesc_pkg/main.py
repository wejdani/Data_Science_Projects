class List_Description(object): 
    
    def __init__(self, some_list):
        self.some_list = some_list
        self.one_digit = {'zero':0,'one':1,'two':2,'three':3,'four':4,'five':5,'six':6,'seven':7,'eight':8,'nine':9,'ten':10,
                          'eleven':11,'twelve':12,'thirteen':13,'fourteen':14,'fifteen':15,'sixtreen':16,'seventeen':17,
                          'eighteen':18,'nineteen':19}
        self.two_digit = {'twenty':20, 'thirty':30, 'forty':40, 'fifty':50,'sixty':60, 'seventy':70, 'eighty':80, 'ninety':90}


    def return_max(self):
        lis = self.word_to_num(self.some_list)    # first we need to convert any strings to numbers  
        maximum = lis[0]                          # store the first number of the list in a variable maximum
        for i in range(len(lis)):                 # loop through the list
            if(maximum<lis[i]):                   # if the maximum we have is less than another element in the list
                maximum = lis[i]                  # re-assign it to store the highest number
        return maximum                            # return the maximum
    
    def return_min(self):
        minimum = self.some_list[0]               # store the first number of the list in a variable maximum
        for i in range(len(self.some_list)):      # loop through the list
            if(minimum>self.some_list[i]):        # if the maximum we have is less than another element in the list
                minimum = self.some_list[i]       # re-assign it to store the highest number
        return minimum                            # return the maximum
        
    def return_max_squared(self):                  
        maximum = self.return_max()               # call the maximum function to get the max
        return maximum**2                         # return the maximum after squaring it
    
    def return_length(self):
        count = 0                                 # define counter
        for i in range(len(self.some_list)):      # loop through the list
            count +=1                             # for every element add 1 to the counter
        return count                              # return counter
    
    def return_positive_sum(self):
        sum = 0                                   # define sum variable
        for i in range(len(self.some_list)):      # loop through the list
            if(self.some_list[i]>0):              # if number is above 0 (positive)
                sum +=self.some_list[i]           # add it to the sum
        return sum                                # return the sum
    
    def return_negative_count(self):
        count = 0                                 # define counter
        for i in range(len(self.some_list)):      # loop through the list
            if(self.some_list[i]<0):              # if number is less than 0 (negative)
                count +=1                         # for every element add 1 to the counter
        return count                              # return counter

# for my first extra method I made a method that returns the unique numbers in the list
    def return_unique(self): 
        unique = set()                            # define set unique
        for i in self.some_list:                  # loop through the list
            if(i not in unique):                  # for every element not already in the set
                unique.add(i)                     # add that item
        return list(unique)                       # return the unique set as a list
    
# for my second extra method, I needed 3 methods to achieve what I wanted to do, which is convert word numbers into integers 
# example: 
# method input: [1,2,4,'twenty','three','sixty-four']
# method output: [1,2,4,20,3,64]
# however there are some restrictions: 
#   1- the spelling has to be as specified in the init above
#   2- the naming convention of the double digits has to be as following: first digit - second digit -> 'thirty-four'
#   3- this method only converts number words from 0 to 99

# this is the first method needed to achieve my goal and it takes a number word string of 1 digit and returns the integer of it
# but it also includes numbers from 10 to 19 because of their different names in english
# this method is for converting the word numbers that are composed of one word ex: 'one','thirty','fourteen'
# it works by looping through the dictionary of one digit numbers and checks if any of the keys match the string passed in, 
# if so it replaces it with  the value of that key which is an integer
    def convert_one_word(self, word_number):
        for k,v in self.one_digit.items():    # loop through the dictionary
            if(word_number==k):               # if the word passed in matches any of the keys
                number=v                      # assign the value of that key to a number
        for k,v in self.two_digit.items():    # this is for the numbers 20,30,40,...etc
            if(word_number==k):               # if the word passed in matches any of the keys
                number=v                      # assign the value of that key to a number
        return number                         # return number

# this is the second method needed to achieve my goal and it takes a number word string of 2 digits and returns the integer of it
    def convert_two_word(self, word_number):
        double_digit = word_number.split('-')                  # split the word by '-' and store it in a list
        second_digit = self.convert_one_word(double_digit[1])  # convert the second digit using the convert_one_word method
        for k,v in self.two_digit.items():                     # loop through the dictionary and find its number
            if(double_digit[0]==k):                            # check using the list storing the separated words
                first_digit=v                                  # store it in variable

        return first_digit+second_digit                        # add both digits to get the number and return
    
# this is the final method which uses the 'convert_one_digit' and the 'convert_two_digit' to achieve the goal of converting number word string to integer
    def word_to_num(self, some_list): 
        for i in range(0,len(some_list)):                              # loop through the list
            if(type(some_list[i])== str):                              # if we find a string
                if(some_list[i].find('-')!=-1):                        # check if its one word or two words
                    some_list[i]=self.convert_two_word(some_list[i])   # if its two words use the convert_two_word method
                else: 
                    some_list[i]=self.convert_one_word(some_list[i])   # otherwise call the convert_one_word method
        return(some_list)                                              # return the list