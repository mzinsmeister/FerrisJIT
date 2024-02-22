import random
import sys

def generate_expression(complexity):
    if complexity <= 1:
        if random.random() < 0.5:
            return f"$0"
        else:
            return str(random.randint(1, 10))
    else:
        op = random.choice(['+', '*'])
        left_complexity = random.randint(1, complexity - 1)
        right_complexity = complexity - 1 - left_complexity
        if op == '+':
            return f"({op} {generate_expression(left_complexity)} {generate_expression(right_complexity)})"
        elif op == '*':
            return f"({op} {generate_expression(left_complexity)} {generate_expression(right_complexity)})"

def main():
    if len(sys.argv) != 2:
        print("Usage: python script.py <total_complexity>")
        sys.exit(1)
    
    total_complexity = int(sys.argv[1])
    expression = generate_expression(total_complexity)
    print(expression)

if __name__ == "__main__":
    main()

