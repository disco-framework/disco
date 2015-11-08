#!/usr/bin/env python3

import json
import select
import sys
import unittest


# write integration tests from GUI perspective here
# use JSON to communicate with framework
# execute with "make integration-test" in project root

# CAREFUL: All the tests depend on each other.
# They are executed in alphabetical order and are only running correctly in
# exactly this order.

# TODO additional test case ideas
  # worker generates some valid propositions
    # last gets counted, worker stays active
  # worker generates invalid proposition
  # load multiple prepared invalid savegame files to test savegame error handling

def read_json():
    line = input()
#    print("input: " + line, file=sys.stderr)
    return json.loads(line)

def clear_mailbox(test_name):
    while select.select([sys.stdin], [], [], 0.01) == ([sys.stdin], [], []):
        line = input()
        print("!! input skipped in \"" + test_name + "\" test: " + line, file=sys.stderr)

class TestDiscoFramework(unittest.TestCase):

    # tests are sorted by name. This is important because these tests are order dependent.
    def test_1_all_data(self):
    # is all_data correctly received on startup?
        msg = read_json()
        self.assertEqual('all data',     msg['event'])
        self.assertEqual('[1]',          msg['state'])
        self.assertEqual(False,          msg['running'])
        self.assertEqual(0,              msg['round'])
        self.assertEqual(0,              msg['problem idx'])
        self.assertEqual(['[10]', '42'], msg['worker input'])
        self.assertEqual(['ten', '[10] 42', 1000, '[1]'],                                 msg['problems'][0])
        self.assertEqual([['pwb_00', 'test-worker', '', None, '', 0, 0, 0, 'no', False]], msg['workers'])

    def test_2_blocking(self):
        clear_mailbox("blocking")
        print(json.dumps({'action': 'block worker', 'worker id': 'pwb_00'}))
        update_msg = read_json()
        self.assertEqual('worker updated',                                 update_msg['event'])
        self.assertEqual(['pwb_00', None, '', 0, 0, 0, {'idx': 1}, False], update_msg['worker data'])

        print(json.dumps({'action': 'unblock worker', 'worker id': 'pwb_00'}))
        update_msg = read_json()
        self.assertEqual('worker updated',                           update_msg['event'])
        self.assertEqual(['pwb_00', None, '', 0, 0, 0, 'no', False], update_msg['worker data'])

    def test_3_start_round(self):
    # worker echoes input: test that the worker gets the right input
        clear_mailbox("start round")
        print(json.dumps({'action': 'start round'}))

        running_msg = read_json()
        self.assertEqual('worker updated',                          running_msg['event'])
        self.assertEqual(['pwb_00', None, '', 0, 0, 0, 'no', True], running_msg['worker data'])

        start_msg = read_json()
        self.assertEqual('round started', start_msg['event'])
        self.assertEqual(1,               start_msg['round number'])

        proposition_msg = read_json()
        self.assertEqual('worker updated', proposition_msg['event'])
        self.assertEqual(['pwb_00', '[(0,0,10)]', '[10] 42', 42, 0, 0, 'no', True], proposition_msg['worker data'])

        not_running_msg = read_json()
        self.assertEqual('worker updated', not_running_msg['event'])
        self.assertEqual(['pwb_00', '[(0,0,10)]', '[10] 42', 42, 0, 0, 'no', False], not_running_msg['worker data'])

        stop_msg = read_json()
        self.assertEqual('round ended', stop_msg['event'])
        self.assertEqual(1,             stop_msg['round number'])

        worker_update_msg = read_json() # "worker updated"
        self.assertEqual('worker updated', worker_update_msg['event'])
        self.assertEqual(['pwb_00', '[(0,0,10)]', '[10] 42', 42, 42, 0, 'no', False], worker_update_msg['worker data'])

        input_change_msg = read_json()
        self.assertEqual('worker input changed', input_change_msg['event'])
        self.assertEqual(['[10]','42'], input_change_msg['worker input'])

    def test_4_save_add_load(self):
        # test saving and loading game state
        # as well as adding scores (we need a state change to test
        # loading the savegame anyway)
        clear_mailbox("save add load")

        filepath = '/tmp/disco-test-savegame.sav'
        filepath2 = '/tmp/disco-test-savegame2.sav'

        # create first savegame
        print(json.dumps({'action': 'save game state', 'file path': filepath}))
        result_msg = read_json()
        self.assertEqual('save game state', result_msg['event'])
        self.assertEqual('ok',              result_msg['result'])

        # change state (add scores)
        print(json.dumps({'action': 'add scores'}))
        worker_updated_msg = read_json()
        self.assertEqual('worker updated', worker_updated_msg['event'])
        self.assertEqual(['pwb_00', '[(0,0,10)]', '[10] 42', 42, 42, 42, 'no', False], worker_updated_msg['worker data'])

        # create second savegame
        print(json.dumps({'action': 'save game state', 'file path': filepath2}))
        result_msg = read_json()
        self.assertEqual('save game state', result_msg['event'])
        self.assertEqual('ok',              result_msg['result'])

        # load first savegame
        print(json.dumps({'action': 'load game state', 'file path': filepath}))
        load_result_msg = read_json()
        self.assertEqual('load game state', load_result_msg['event'])
        self.assertEqual('ok', load_result_msg['result'])

        # worker should be in pre-add state
        full_state_msg = read_json()
        self.assertEqual('all data', full_state_msg['event'])
        self.assertEqual([['pwb_00', 'test-worker', '', '[(0,0,10)]', '[10] 42', 42, 42, 0, 'no', False]],
                         full_state_msg['workers'])

        # load second save game
        print(json.dumps({'action': 'load game state', 'file path': filepath2}))
        load_result_msg = read_json()
        self.assertEqual('load game state', load_result_msg['event'])
        self.assertEqual('ok', load_result_msg['result'])

        # worker should have added scores
        full_state_msg = read_json()
        self.assertEqual('all data', full_state_msg['event'])
        self.assertEqual([['pwb_00', 'test-worker', '', '[(0,0,10)]', '[10] 42', 42, 42, 42, 'no', False]],
                         full_state_msg['workers'])

    def test_5_choose_problemand_manually_end_round(self):
        clear_mailbox("choose problem and manually end round")
        print(json.dumps({'action': 'choose problem', 'problem idx': 1}))
        update_msg = read_json()
        self.assertEqual('problem chosen', update_msg['event'])
        self.assertEqual(1,                update_msg['problem idx'])

        state_changed_msg = read_json() # "problem state changed"
        self.assertEqual('problem state changed', state_changed_msg['event'])
        self.assertEqual('[1]',                   state_changed_msg['problem state'])

        input_changed_msg = read_json() # "worker input changed"
        self.assertEqual('worker input changed', input_changed_msg['event'])
        self.assertEqual(['[20]', '42'],         input_changed_msg['worker input'])

        # TODO find out why this is necessary. The worker seems to be in a bad state. Why?
        print(json.dumps({'action': 'kill all workers'}))

        # start round
        print(json.dumps({'action': 'start round'}))
        self.assertEqual('worker updated', read_json()['event']) # worker running
        self.assertEqual('round started', read_json()['event'])
        self.assertEqual('worker updated', read_json()['event']) # worker submitted proposition

        # manually end round
        print(json.dumps({'action': 'kill all workers'}))

        not_running_msg = read_json()
        self.assertEqual('worker updated', not_running_msg['event'])
        self.assertEqual(['pwb_00', '[(0,0,20)]', '[20] 42', 42, 0, 42, 'no', False], not_running_msg['worker data'])

        stop_msg = read_json()
        self.assertEqual('round ended', stop_msg['event'])
        self.assertEqual(1,             stop_msg['round number'])

        worker_update_msg = read_json()
        self.assertEqual('worker updated', worker_update_msg['event'])
        self.assertEqual(['pwb_00', '[(0,0,20)]', '[20] 42', 42, 42, 42, 'no', False], worker_update_msg['worker data'])

        input_change_msg = read_json()
        self.assertEqual('worker input changed', input_change_msg['event'])
        self.assertEqual(['[20]','42'], input_change_msg['worker input'])

    def test_6_apply_proposition(self):
        clear_mailbox("apply proposition")
        print(json.dumps({'action': 'apply proposition', 'worker id': 'pwb_00'}))

        message = read_json()
        self.assertEqual('problem state changed', message['event'])
        self.assertEqual('[1]\n[(0,0,20)]', message['problem state'])

        message = read_json()
        self.assertEqual('worker input changed', message['event'])
        self.assertEqual(['[20]', '42'], message['worker input'])


if __name__ == '__main__':
    # execute tests
    unittest.main(exit=False, failfast=True)
    sys.stderr.flush()

    # shut down framework
    print(json.dumps({'action': 'quit program'}))
    sys.stdout.flush()

    # wait for framework to shut down before exiting test suite
    while True:
        sys.stdin.readline()
