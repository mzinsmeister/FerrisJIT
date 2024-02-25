import random
import sys

def generate_expression(complexity, last_op=None, no_variables=False):
    if complexity <= 1:
        if not no_variables and random.random() < 0.5:
            return f"$0"
        else:
            if last_op == '/':
                return str(random.randint(1, 10))
            elif last_op == '-':
                return str(random.randint(1, 100))
            elif last_op == '*':
                return str(random.randint(1, 4))
            else:
                return str(random.randint(1, 10))
    elif complexity <= 3:  # For complexity 2 or 3, allow all operations
        op = random.choice(['+', '-', '*'])
        return f"({op} {generate_expression(1)} {generate_expression(1, no_variables=True)})"
    else:
        op = random.choices(['+', '-', '*'], weights=[2, 1, 1])[0]  # Adjust weights for subtraction, multiplication, and equality
        left_complexity = random.randint(1, complexity - 1)
        right_complexity = complexity - 1 - left_complexity
        return f"({op} {generate_expression(left_complexity, op)} {generate_expression(right_complexity, op)})"

def generate_boolean_expression(complexity):
    left_complexity = random.randint(1, complexity - 1)
    right_complexity = complexity - 1 - left_complexity
    return f"(= {generate_expression(left_complexity)} {generate_expression(right_complexity)})"

def main():
    if len(sys.argv) != 2:
        print("Usage: python script.py <total_complexity>")
        sys.exit(1)
    
    total_complexity = int(sys.argv[1])
    if random.random() < 0.3:
        expression = generate_boolean_expression(total_complexity)
    else:
        expression = generate_expression(total_complexity)
    print(expression)

if __name__ == "__main__":
    main()


