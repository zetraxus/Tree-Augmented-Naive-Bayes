with open('results') as input, open('results_dir/precision', 'w') as precision, open('results_dir/recall', 'w') as recall, open('results_dir/f1',
                                                                                                        'w') as f1, \
        open('results_dir/precision_w', 'w') as precision_w, open('results_dir/recall_w', 'w') as recall_w, open('results_dir/f1_w', 'w') as f1_w:

    i, dataset_algorithm, max_values, best_algorithms = 0, "", [0, 0, 0, 0, 0, 0], ["", "", "", "", "", ""]
    for row in input:
        if i % 7 == 0:
            dataset_algorithm = row.replace(' ', ';', 1).replace(' ', '_').replace('\n', '')
        else:
            row = float(row.replace('\n', '').replace('\'', ''))
            if max_values[(i % 7) - 1] < row:
                max_values[(i % 7) - 1] = row
                best_algorithms[(i % 7) - 1] = dataset_algorithm

        if i == 42 or i == 84 or i == 119:
            precision.write(best_algorithms[0] + ";" + str(max_values[0]).replace('.', ',') + '\n')
            recall.write(best_algorithms[1] + ";" + str(max_values[1]).replace('.', ',') + '\n')
            f1.write(best_algorithms[2] + ";" + str(max_values[2]).replace('.', ',') + '\n')
            precision_w.write(best_algorithms[3] + ";" + str(max_values[3]).replace('.', ',') + '\n')
            recall_w.write(best_algorithms[4] + ";" + str(max_values[4]).replace('.', ',') + '\n')
            f1_w.write(best_algorithms[5] + ";" + str(max_values[5]).replace('.', ',') + '\n')
            max_values, best_algorithms = [0, 0, 0, 0, 0, 0], ["", "", "", "", "", ""]
        if i == 119:
            i = 0

        i += 1
