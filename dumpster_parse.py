#!/usr/bin/env python3

# TODO maybe use python3-acora, fast searching for text.

import re
import string
import threading

class EBNFError(Exception):
    pass

class TokenStream:

    def __init__(self, token_string):
        parts = re.split(r"""("[^"]*"|'[^']*')""", token_string)
        # The str.split removes whitespace, if significant whitespace is required,
        # TODO change this.
        parts[::2] = map(str.split, parts[::2])  # outside quotes
        self.ts_words = []
        for part in parts:
            if type(part) is list:
                self.ts_words.extend(part)
            else:
                self.ts_words.append(part)

        self.ts_letters = "".join(self.ts_words)
        # print(self.ts_letters)
        self.index = 0

    def pos(self):
        return self.index

    def seek(self, i):
        self.index = i

    def seek_end(self):
        self.index = len(self.ts_letters)

    def reset(self):
        self.index = 0

    def peek(self, dist=0):
        try:
            out = self.ts_letters[self.index + dist]
            return out
        except IndexError:
            return None

    def eos(self):
        """End of Stream"""
        try:
            val = self.ts_letters[self.index]
            return False
        except IndexError:
            return True

    def consume(self, c):
        try:
            val = self.ts_letters[self.index]
            self.index += 1
            if type(c) is list:
                if val in c:
                    return
                else:
                    self.index -= 1
                    err = EBNFError("Unexpected character {}.".format(val))
                    err.exp = c
                    raise err

            elif type(c) is str:
                if val != c:
                    self.index -= 1
                    err = EBNFError("{} expected, got {}.\n{}".format(c, val, self.ts_letters[self.index - 3:self.index + 3]))
                    err.exp = c
                    raise err
                else:
                    return
            else:
                raise ValueError("Consume only accepts a string or a list of values")

        except IndexError:
            err = EBNFError("Unexpected End of Stream, expected {}.".format(c))
            err.exp = c
            raise err

    def consumel(self, c):

        return lambda: self.consume(c)


class Branch:

    def __init__(self, pos):
        self.pos = pos


class EBNF:
    """Extended Backus-Naur Form"""

    def __init__(self, filename):

        self.fname = filename

        with open(filename, "rt") as f:
            self.rawfile = f.read()

        self.ts = TokenStream(self.rawfile)

        try:
            self.check_syntax1()
            print("check_syntax1 completed successfully!")
        except EBNFError as e:
            print(e)

        try:
            self.check_syntax2()
            print("check_syntax2 completed successfully!")
        except EBNFError as e:
            print(e)

    def check_exp(self, f):

        if not callable(f):
            raise ValueError()

        save = self.ts.index

        self.ts.seek_end()

        out = ""

        try:
            f()
        except EBNFError as e:
            out = e.exp

        self.ts.index = save

        return out

    def sequence(self, flist):

        if not all(map(callable, flist)):
            raise ValueError()

        for f in flist:
            f()

    def seql(self, flist):

        return lambda: self.sequence(flist)

    def repetition(self, f):

        if type(f) is list:
            if not all(map(callable, f)):
                raise ValueError()

            try:
                while True:
                    for func in f:
                        func()
            except EBNFError:
                pass
        else:
            if not callable(f):
                raise ValueError()

            try:
                while True:
                    f()
            except EBNFError:
                pass

    def repl(self, f):
        return lambda: self.repetition(f)

    def alternation(self, options):

        val = self.ts.peek()

        if not all(map(callable, options)):
            raise ValueError()

        exp = {}

        # Go through each of the alternate options and see what their expected symbol is.
        for opt in options:
            res = self.check_exp(opt)

            if type(res) is list:
                exp[opt] = res
            else:
                exp[opt] = [res]

        for opt in options:
            if val in exp[opt]:
                opt()
                return

        err = EBNFError("None of the possible alternations qualify.")
        err.exp = [item for sublist in exp.values() for item in sublist]
        raise err

    def altl(self, options):
        return lambda: self.alternation(options)

    def check_syntax1(self):

        self.repetition(self.check_gap_separator)

        self.check_gap_free_symbol()

        self.repetition(self.check_gap_separator)

        self.repetition(
            [self.check_gap_free_symbol, self.repl(self.check_gap_separator)]
        )

        self.ts.reset()

    def check_gap_separator(self):
        self.ts.consume([' ', '\t', '\n', '\v', '\f'])

    def check_gap_free_symbol(self):

        terminal_character = string.ascii_letters + string.digits + ',=|/!*))]}-\'"?(*([{;. :+_%@&#$<>\\^`~'

        first_terminal_character = list(terminal_character.replace("'", "", 1))
        second_terminal_character = list(terminal_character.replace('"', "", 1))

        self.alternation(
            [self.ts.consumel(first_terminal_character),
             self.ts.consumel(second_terminal_character),
             self.check_terminal_string]
        )

    def check_terminal_string(self):

        terminal_character = string.ascii_letters + string.digits + ',=|/!*))]}-\'"?(*([{;. :+_%@&#$<>\\^`~'

        first_terminal_character = list(terminal_character.replace("'", "", 1))
        second_terminal_character = list(terminal_character.replace('"', "", 1))

        self.alternation(
            [
                self.seql(
                    [
                        self.ts.consumel("'"),
                        self.ts.consumel(first_terminal_character),
                        self.repl(self.ts.consumel(first_terminal_character)),
                        self.ts.consumel("'")
                    ]
                )
                ,
                self.seql(
                    [
                        self.ts.consumel('"'),
                        self.ts.consumel(second_terminal_character),
                        self.repl(self.ts.consumel(second_terminal_character)),
                        self.ts.consumel('"')
                    ]
                )
            ]
        )

    def check_syntax2(self):
        self.check_syntax_rule()
        self.repetition(self.check_syntax_rule)
        self.ts.reset()

    def check_syntax_rule(self):
        self.check_meta_identifier()
        self.ts.consume('=')
        self.check_definitions_list()
        self.ts.consume([';', '.'])

    def check_meta_identifier(self):
        # meta identifier = letter, {meta identifier character} ;
        # meta identifier character = letter | decimal digit;
        self.ts.consume(list(string.ascii_letters))
        self.repetition(self.ts.consumel(list(string.ascii_letters + string.digits)))

    def check_definitions_list(self):
        self.check_single_definition()
        self.repetition(self.seql(
            [
                self.ts.consumel(['|', '/', '!']),
                self.check_single_definition
            ]
        ))

    def check_single_definition(self):
        # single definition = syntactic term, {concatenate symbol, syntactic term} ;

        self.check_syntactic_term()
        self.repetition(self.seql(
            [
                self.ts.consumel(','),
                self.check_syntactic_term
            ]
        ))

    @staticmethod
    def optional(f):

        if not callable(f):
            raise ValueError()

        try:
            f()
        except EBNFError:
            pass

    @staticmethod
    def optl(f):

        return lambda: EBNF.optional(f)

    def check_syntactic_term(self):
        # syntactic term = syntactic factor [except symbol, syntactic exception];

        self.check_syntactic_factor()
        EBNF.optional(
            self.seql(
                [
                    self.ts.consumel('-'),
                    self.check_syntactic_exception
                ]
            )
        )

    def check_integer(self):
        # integer = decimal digit, {decimal digit} ;
        self.ts.consume(list(string.digits))

        self.repetition(
            self.ts.consumel(list(string.digits))
        )

    def check_syntactic_factor(self):

        # syntactic factor = [integer, repetition symbol], syntactic primary;
        EBNF.optional(
            self.seql(
                [
                    self.check_integer,
                    self.ts.consumel('*')
                ]
            )
        )
        self.check_syntactic_primary()

    def check_syntactic_primary(self):
        """syntactic primary = optional sequence
                  | repeated sequence
                  | grouped sequence
                  | meta identifier
                  | terminal string
                  | special sequence
                  | empty sequence ;"""

        self.alternation(
            [
                self.check_optional_sequence,
                self.check_repeated_sequence,
                self.check_grouped_sequence,
                self.check_meta_identifier,
                self.check_terminal_string,
                self.check_special_sequence,
                self.check_empty_sequence,
            ]
        )

    def check_optional_sequence(self):

        self.ts.consume("[")
        self.check_definitions_list()
        self.ts.consume("]")

    def check_repeated_sequence(self):

        self.ts.consume("[")
        self.check_definitions_list()
        self.ts.consume("]")

    def check_grouped_sequence(self):

        self.ts.consume("[")
        self.check_definitions_list()
        self.ts.consume("]")

    def check_special_sequence(self):
        #special sequence = special sequence symbol, {special sequence character}, special sequence symbol ;
        # special sequence character = terminal character - special sequence symbol;

        terminal_character = string.ascii_letters + string.digits + ',=|/!*))]}-\'"?(*([{;. :+_%@&#$<>\\^`~'
        special_sequence_character = list(terminal_character.replace('?', '', 1))

        self.ts.consume("?")
        self.repetition(self.ts.consumel(special_sequence_character))
        self.ts.consumel("?")

    def check_empty_sequence(self):
        pass

    def check_syntactic_exception(self):
        pass


a = EBNF("ebnf_verbose.ebnf")