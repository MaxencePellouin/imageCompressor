##
## EPITECH PROJECT, 2022
## wolfram
## File description:
## Makefile
##

NAME = imageCompressor

SB = stack build

SP := $(shell stack path --local-install-root)

$(NAME):
	@echo "Compiling $(NAME)"
	@$(SB)
	@cp -r $(SP)/bin/hpack-exe $(NAME)
	@echo "Compilation done"

all: $(NAME)

clean:
	@echo "Cleaning"
	@stack clean
	@echo "Done"

fclean: clean
	@rm -f $(NAME)

re: fclean all

.PHONY: all clean fclean re