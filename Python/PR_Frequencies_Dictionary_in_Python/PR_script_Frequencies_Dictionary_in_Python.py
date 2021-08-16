## JOAO MARTINS N.93259

import numpy

## Classe 1: Entrada do dicionário (Entry)

class Entry:
    def __init__(self, word):
        self.__word = word
        self.__frequency = 1 #a partir do momento em que determinada palavra ocorre pela primeira vez, a sua frequência é 1


    @property
    def word(self):     #para mostrar qual a palavra
        return self.__word

    @property
    def frequency(self):    #para mostrar a frequência com que a palavra ocorre
        return self.__frequency


    def increase_frequency(self):      #para incrementar por 1 a frequência com que a palavra ocorre
        self.__frequency = self.__frequency + 1


    def __eq__(self, other):        #para verificar se a entrada(Entry) é igual a outra introduzida
        if not isinstance(other, self.__class__):
            return False
        return self.__word == other.__word


#####################################################################################################################
## Classe 2: Dicionário de Frequências (Dictionary)


class Dictionary:
    def __init__(self, filename):
        self.__list_of_entries = []
        self.update_dictionary(filename)

## Métodos da Classe 2

    ##1 Atualizar o dicionário de frequências com base num ficheiro de texto, dado o seu nome.
    def update_dictionary(self, filename):
        f = open(filename, "r")
        for line in f:
            for word in line.split():
                word = Entry(word)
                is_in_list = False
                for i in range(len(self.__list_of_entries)):
                    if word.word.lower() == self.__list_of_entries[i].word.lower():
                        is_in_list = True
                        self.__list_of_entries[i].increase_frequency()
                if is_in_list == False:
                    self.__list_of_entries.append(word)


    ##2 Remover uma entrada do dicionário, dada a palavra.
    def remove_entry(self, word):
        i = 0
        while i != len(self.__list_of_entries):
            if self.__list_of_entries[i].word.lower() == word.lower():
                del self.__list_of_entries[i]
                return None # para que salte fora do loop sem dar erros (quando elimina uma entrada, len reduz 1)
            i = i + 1


    ##3 Remover as entradas com frequência abaixo de uma frequência dada.

    def remove_below_frequency(self, freq):
        i = 0
        erase_list = []
        while i != len(self.__list_of_entries):
            if self.__list_of_entries[i].frequency < freq:
                erase_list.append(i)
            i = i + 1
        for i in range(len(erase_list)):
            del self.__list_of_entries[erase_list[i]]


    ## 4 Devolver a frequência de uma palavra, dada a palavra.
    def frequency_of_word(self, word):
        i = 0
        while i != len(self.__list_of_entries):
            if self.__list_of_entries[i].word.lower() == word.lower():
                return self.__list_of_entries[i].frequency
            i = i + 1
        return 0


    ## 5 Devolver a frequência relativa de uma palavra (frequência da palavra a dividir pelo total de frequências), dada a palavra.
    def relative_frequency_of_word(self, word):
        if isinstance(self.frequency_of_word(word.lower()), str):
            return 0
        else:
            return self.frequency_of_word(word) / self.total_frequencies()


    ## 6 Devolver o total de frequencias.
    def total_frequencies(self):
        result = 0
        for i in range(len(self.__list_of_entries)):
            result = result + self.__list_of_entries[i].frequency
        return result


    ## 7 Devolver todas as palavras com frequ^encias entre um limite m´ınimo e limite m´aximo dados.
    def words_between_occurrences(self, min, max):
        entries_between_occurrences = []
        for i in range(len(self.__list_of_entries)):
            if self.__list_of_entries[i].frequency >= min:  # não colocquei if aaaaa AND bbbbb na mesma linha por recomendação do próprio pycharm
                if self.__list_of_entries[i].frequency <= max:
                    entries_between_occurrences.append(self.__list_of_entries[i].word)
        return entries_between_occurrences


    ## 8 Devolver as n palavras mais frequentes, dado n.
    def n_most_frequent(self, n):
        frequencies_vector = numpy.zeros(len(self.__list_of_entries), int)
        for i in range(len(self.__list_of_entries)):
            frequencies_vector[i] = self.__list_of_entries[i].frequency
        n_most_frequent = []
        i = 0
        while i < n and i < len(self.__list_of_entries):
            n_most_frequent.append(self.__list_of_entries[frequencies_vector.argmax()].word)
            frequencies_vector[frequencies_vector.argmax()] = 0
            i = i + 1
        return n_most_frequent


    ## 9 Devolver a palavra mais comprida.
    def longest_word(self):
        longest_word = self.__list_of_entries[0].word
        for i in range(len(self.__list_of_entries)):
            if len(self.__list_of_entries[i].word) > len(longest_word):
                longest_word = self.__list_of_entries[i].word
        return longest_word


    ## 10 Devolver a palavra mais curta.
    def shortest_word(self):
        shortest_word = self.__list_of_entries[0].word
        for i in range(len(self.__list_of_entries)):
            if len(self.__list_of_entries[i].word) < len(shortest_word):
                shortest_word = self.__list_of_entries[i].word
        return shortest_word


    ## 11 Escrever no ecra o dicionario em forma de histograma
    def histogram(self):
        for i in range(len(self.__list_of_entries)):
            print(self.__list_of_entries[i].word, ": ", end='')
            for j in range(self.__list_of_entries[i].frequency):
                print("*", end='')
            print('')


    ## 12 Devolver a representação vetorial de um documento, dado o seu nome.
    def vector_representation(self, filename):
        f = open(filename, "r")
        vector_rep = numpy.zeros(len(self.__list_of_entries), int)
        for line in f:
            for word in line.split():
                i = 0
                while i != len(self.__list_of_entries):
                    if self.__list_of_entries[i].word == word:
                        vector_rep[i] = self.__list_of_entries[i].frequency
                    i = i + 1
        return vector_rep


    ##13 Devolver a similaridade entre dois documentos dados os seus nomes. A similaridade entre dois documentos pode ser
    # obtida calculando o coseno entre os vetores que os representam
    def document_similarity(self, filename1, filename2):
        vector_u = self.vector_representation(filename1)
        vector_v = self.vector_representation(filename2)
        #usando numpy.inner(x,y) para obter o produto interno de ambos os vectores e numpy.linalg.norm(x) para obter o módulo de cada vector, temos:
        return numpy.inner(vector_u, vector_v) / ((numpy.linalg.norm(vector_u) * numpy.linalg.norm(vector_v)))


    ##14 Devolver uma matriz de similaridade entre duas listas de documentos dadas.
    def simialrity_matrix(self, documents_list1, documents_list2):
        similarity_matrix = numpy.zeros([len(documents_list1), len(documents_list2)])
        for i in range(0, similarity_matrix.shape[0]):
            for j in range(0, similarity_matrix.shape[1]):
                similarity_matrix[i, j] = self.document_similarity(documents_list1[i], documents_list2[j])
        return similarity_matrix


## JOAO MARTINS N.93259
