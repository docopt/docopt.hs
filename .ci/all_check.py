#!/usr/bin/env python3
"""
Makes sure:
 * All jobs are listed in the 'all' job
 * Only existing tests are listed
"""

import sys
import yaml

CI_PATH = '.github/workflows/ci.yaml'
ALL_TEST = 'all'


def main():
    ci_yaml_fp = open(CI_PATH, 'r')
    ci_yaml_parsed = yaml.load(ci_yaml_fp, Loader=yaml.FullLoader)

    all_jobs = set(ci_yaml_parsed['jobs'].keys()) - {ALL_TEST}
    all_needs = set(ci_yaml_parsed['jobs'][ALL_TEST]['needs'])

    if all_jobs - all_needs:
        sys.exit(f'Not all jobs mentioned in {ALL_TEST}.needs: '
                 f'{all_jobs - all_needs}')

    if all_needs - all_jobs:
        sys.exit(f'Non-existing jobs found in {ALL_TEST}.needs: '
                 f'{all_needs - all_jobs}')


if __name__ == '__main__':
    main()
