import math

def is_category(column):
    return column.dtype.name == 'category'


def is_object(column):
    return column.dtype.name == 'object'


def get_column_bytype(dataframe, type):
    return [column for column in dataframe.columns if column.dtype.name == type]

# For binning continuous variables
def bin(start_range, end_range, no_of_bins):
    add_value = (end_range - start_range)/no_of_bins
    temp=start_range
    list = []
    for i in range(no_of_bins):
        list.append(math.floor(temp))
        temp = temp+add_value
    return(list)


def imputeCategoricalVariables(Data):
    for i in Data.columns:
        if Data[i].dtype.name == 'category':
            Data.loc[Data[i].isnull(), [i]] = Data[i].mode()[0]
            print(i, Data[i].mode()[0])


def get_sum_order(data, group_by, sort_by, ascending):
    return data.groupby([group_by]).sum().sort_values(by=[sort_by], ascending=ascending).index

def get_mean_order(data, group_by, sort_by, ascending):
    return data.groupby([group_by]).mean().sort_values(by=[sort_by], ascending=ascending).index

def get_count_order(data, group_by, sort_by, ascending):
    return data.groupby([group_by]).count().sort_values(by=[sort_by], ascending=ascending).index
