import sqlite3
import argparse
import re
import sexpdata
import json


# Different in that they enables full text search
# create_meanings_table = \
#                         """CREATE VIRTUAL TABLE [VocabMeaningSet] USING fts5 (
#                         [ID] integer NOT NULL PRIMARY KEY AUTOINCREMENT,
#                         [Meaning] nvarchar(600));"""
# create_kanji_meanings_table = \
#                               """CREATE VIRTUAL TABLE [KanjiMeaningSet] USING fts5 (
                              

#                               [ID] integer NOT NULL PRIMARY KEY AUTOINCREMENT,
#                               [Language] nvarchar(10),
#                               [Meaning] nvarchar(300) NOT NULL,
#                               [Kanji_ID] integer NOT NULL);"""


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('source')
    parser.add_argument('out')
    parser.add_argument('--text', action='store_true')
    args = parser.parse_args()

    with sqlite3.connect(args.source) as conn, get_output(args) as out:
        src_cur = conn.cursor()
        src_cur.execute('select * from sqlite_master')
        src_master = src_cur.fetchall()
        src_master = [convert_schema_entry(e) for e in src_master if e[4] is not None]
        src_master = [e for e in src_master if 'sqlite' not in e['sql']]
        src_tables = filter(lambda r: r['type'] == 'table', src_master)
        src_indices = filter(lambda r: r['type'] == 'index' and r['sql'] is not None, src_master)

        for table in src_tables:
            maybe_execute(table['sql'], out, args.text)
            src_cur.execute('SELECT * FROM %s' % table['name'])
            cols = [d[0] for d in src_cur.description]
            cols_map = {c: idx for idx, c in enumerate(cols)}
            for row in src_cur:
                query = 'INSERT INTO %(tbl)s (%(cols)s) VALUES (%(phold)s)' % {
                    'tbl': table['name'],
                    'cols': ','.join(cols),
                    'phold': ','.join(make_sqlite_string_literal(convert_data_value(row[cols_map[col]])) for col in cols)
                }
                #dst_cur.execute(query, [convert_data_value(row[cols_map[col]]) for col in cols])
                maybe_execute(query, out, args.text)

            if not args.text:
                out[0].commit()

            table_idx = filter(lambda r: r['tbl_name'] == table['name'], src_indices)
            for idx in table_idx:
                maybe_execute(idx['sql'], out, args.text)


def get_output(args):
    if args.text:
        return open(args.out, mode='w')
    else:
        conn = sqlite3.connect(args.out)
        cursor = conn.cursor()
        return conn, cursor


def make_sqlite_string_literal(string):
    return "'{}'".format(string.replace("'", "''"))


def maybe_execute(command, maybe_cursor_maybe_file, print_command):
    if print_command:
        maybe_cursor_maybe_file.write(json.dumps(command))
        maybe_cursor_maybe_file.write('\n')
    else:
        maybe_cursor_maybe_file[1].execute(command)


def convert_schema_entry(entry):
    r = dict()
    r['type'] = entry[0]
    match = re.match(r'^CREATE (TABLE|INDEX) \[?([^ \]\(]+)\]? ?', entry[4])
    r['name'] = match.group(2)

    # table_creation_substitutions = {'VocabMeaningSet': create_meanings_table,
    #                                 'KanjiMeaningSet': create_kanji_meanings_table}
    # r['sql'] = table_creation_substitutions.get(r['name'], entry[4])
    r['sql'] = entry[4]

    match = re.search(r'ON \[([^\]]+)\]', entry[4])
    if match:
        r['tbl_name'] = match.group(1)
    elif r['type'] == 'index':
        print(entry)
        exit()
    return r


def convert_data_value(value):
    if isinstance(value, str):
        return sexpdata.dumps(value)
    else:
        return value


if __name__ == '__main__':
    main()
