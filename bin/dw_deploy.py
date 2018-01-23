# Markdown to DW deployment script
# inspired by https://github.com/cvut/MI-PYT
import os

import click
import pypandoc
import requests

from lxml import etree


class DW:

    def __init__(self, url, username, password):
        self.url = url + '/doku.php'
        self.session = self._init_session(username, password)

    @staticmethod
    def _find_elem(content, type, attr, value):
        tree = etree.HTML(content)
        for elem in tree.findall('.//' + type):
            if elem.attrib.get(attr) == value:
                return elem
        return None

    def _init_session(self, username, password):
        session = requests.Session()

        params = {'id': 'start', 'do': 'login'}
        r = session.get(self.url, params=params)
        sectok_input = self._find_elem(r.text, 'input', 'name', 'sectok')
        if sectok_input is None:
            raise ValueError('Could not find sectok on login page')

        sectok = sectok_input.attrib.get('value', '')
        login_data = {
            'sectok': sectok,
            'id': 'start',
            'do': 'login',
            'authnProvider': '2',
            'u': username,
            'p': password,
            'r': '1',
        }
        params['sectok'] = sectok
        r = session.post(self.url, params=params, data=login_data)
        if 'logout' not in r.text:
            raise ValueError('Could not login')

        return session

    def put_page(self, dw_page, content):
        params = {'id': dw_page, 'do': 'edit'}
        r = self.session.get(self.url, params=params)

        edit_form = self._find_elem(r.text, 'form', 'id', 'dw__editform')
        if edit_form is None:
            raise ValueError('Could not find edit form on parsed page')

        data = {}
        for inp in edit_form.findall('.//input'):
            name = inp.attrib.get('name')
            if not name.startswith('do['):
                data[name] = inp.attrib.get('value', '')

        data['wikitext'] = content
        data['do[save]'] = 'Yes, please!'

        self.session.post(r.url, data=data)

    def put_md(self, dw_page, file):
        self.put_page(dw_page, self._transform_md2dw(file))

    @staticmethod
    def _transform_md2dw(file):
        #: TODO: customizations (images, code haskell, ...)
        return pypandoc.convert_file(file, 'dokuwiki')

    @staticmethod
    def join(*args):
        return ':'.join(args)


def get_files(root, extension):
    files = dict()
    ext_mlen = -len(extension)
    for file in os.listdir(root):
        path = os.path.join(root, file)
        if os.path.isfile(path) and file.endswith(extension):
            files[file[0:ext_mlen]] = path
    return files


@click.command()
@click.argument('root', type=click.Path(exists=True))
@click.option('-d', '--dw-url', help='DokuWiki URL',
              envvar='DW_URL', required=True)
@click.option('-u', '--dw-username', help='DokuWiki username',
              envvar='DW_USERNAME', required=True)
@click.option('-p', '--dw-password', help='DokuWiki password',
              envvar='DW_PASSWORD', required=True)
@click.option('-n', '--dw-namespace', help='Target DokuWiki namespace',
              envvar='DW_NAMESPACE', required=True)
def cli(root, dw_url, dw_username, dw_password, dw_namespace):
    dw = DW(dw_url, dw_username, dw_password)
    markdowns = get_files(root, '.md')
    print('Deploying {} markdown file(s) to {}'.format(
        len(markdowns), dw_url
    ))
    for name, file in markdowns.items():
        qname = dw.join(dw_namespace, name)
        print('| ', file, '-->', qname)
        dw.put_md(qname, file)
    print('Done!')


if __name__ == '__main__':
    cli()
