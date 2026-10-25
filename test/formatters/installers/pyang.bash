pip3 install --break-system-packages pyang

# https://github.com/mbj4668/pyang/issues/958
sed -e 's/import pkgutil/import importlib/' \
    -e 's/pkgutil.find_loader/importlib.util.find_spec/' \
    -i /usr/local/lib/python3*/dist-packages/pyang/repository.py
