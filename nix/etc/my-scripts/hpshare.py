#!/usr/bin/env python3
# /// script
# requires-python = ">=3.12"
# dependencies = [
#     "boto3>=1.42.85",
#     "requests>=2.33.1",
# ]
# ///

import argparse
import logging
import os
import sys
import uuid
from pathlib import Path

import boto3
import requests

logging.basicConfig(level=logging.INFO, format="%(levelname)s: %(message)s")
log = logging.getLogger(__name__)


def get_arg(args, attr, env_name):
    val = getattr(args, attr, None)
    if not val:
        log.error("--{} or {} is required".format(attr.replace("_", "-"), env_name))
        sys.exit(1)
    return val


def create_short_url(shlink_url, api_key, long_url):
    resp = requests.post(
        f"{shlink_url.rstrip('/')}/rest/v2/short-urls",
        headers={"X-Api-Key": api_key, "Content-Type": "application/json"},
        json={"longUrl": long_url},
        timeout=10,
    )
    resp.raise_for_status()
    return resp.json()["shortUrl"].replace('http://', 'https://')


def main():
    parser = argparse.ArgumentParser(description="Upload a file to S3 and get a short link")
    parser.add_argument("file", help="File to upload")
    parser.add_argument("-n", "--no-upload", action="store_true",
                        help="Skip upload; print curl command instead")
    parser.add_argument("-p", "--persistent", action="store_true",
                        help="Do not tag for auto-deletion (default: tag with auto-delete)")
    parser.add_argument("--bucket", default=os.environ.get("HPSHARE_BUCKET", "hpshare"),
                        help="S3 bucket name [env: HPSHARE_BUCKET]")
    parser.add_argument("--region", default=os.environ.get("HPSHARE_REGION", "cn-beijing"),
                        help="S3 region [env: HPSHARE_REGION]")
    parser.add_argument("--endpoint-url", default=os.environ.get("AWS_ENDPOINT_URL", "https://oss-cn-beijing.aliyuncs.com"),
                        help="S3 endpoint URL [env: AWS_ENDPOINT_URL]")
    parser.add_argument("--base-url", default=os.environ.get("HPSHARE_BASE_URL", "https://hpshare.oss-cn-beijing.aliyuncs.com"),
                        help="Public base URL for downloads [env: HPSHARE_BASE_URL]")
    parser.add_argument("--shlink-url", default=os.environ.get("HPSHARE_SHLINK_URL", "https://s.z1k.dev"),
                        help="Shlink instance URL [env: HPSHARE_SHLINK_URL]")
    parser.add_argument("--shlink-api-key", default=os.environ.get("HPSHARE_SHLINK_API_KEY"),
                        help="Shlink API key [env: HPSHARE_SHLINK_API_KEY]")
    args = parser.parse_args()

    bucket     = args.bucket
    base_url   = args.base_url
    shlink_url = args.shlink_url
    shlink_key = get_arg(args, "shlink_api_key", "HPSHARE_SHLINK_API_KEY")

    file_path = Path(args.file)
    if not args.no_upload and not file_path.exists():
        log.error("file not found: %s", args.file)
        sys.exit(1)

    object_key = f"{uuid.uuid4()}/{file_path.name}"
    tagging    = None if args.persistent else "auto-delete=true"

    s3_kwargs = {}
    if args.region:
        s3_kwargs["region_name"] = args.region
    if args.endpoint_url:
        s3_kwargs["endpoint_url"] = args.endpoint_url
    s3 = boto3.client("s3", **s3_kwargs,
                      config=boto3.session.Config(
                          signature_version="s3v4",
                          s3={"addressing_style": "virtual"},
                      ))

    # Presigned PUT URL (valid 24h)
    put_params = {"Bucket": bucket, "Key": object_key}
    if tagging:
        put_params["Tagging"] = tagging
    presigned_url = s3.generate_presigned_url("put_object", Params=put_params, ExpiresIn=86400)

    # Build upload headers (same whether curl or requests)
    # Note: Content-Type is intentionally omitted — it's not in SignedHeaders,
    # so including it would break the presigned URL signature.
    upload_headers = {}
    if tagging:
        upload_headers["x-amz-tagging"] = tagging

    # Public download URL
    download_url = f"{base_url.rstrip('/')}/{object_key}"

    # Short link (always created)
    log.info("Creating short link...")
    try:
        short_url = create_short_url(shlink_url, shlink_key, download_url)
        log.info("Short link created: %s", short_url)
    except Exception as e:
        log.warning("Failed to create short link: %s", e)
        short_url = None

    if args.no_upload:
        header_args = " ".join(f'-H "{k}: {v}"' for k, v in upload_headers.items())
        log.info("Skipping upload. Run this command to upload:\n\n" +
                 "curl -X PUT %s \"%s\" --data-binary @%s\n\n",
                 header_args, presigned_url, args.file)
    else:
        log.info("Uploading %s ...", file_path.name)
        with open(file_path, "rb") as f:
            resp = requests.put(presigned_url, headers=upload_headers, data=f)
        resp.raise_for_status()
        log.info("Upload complete")

    print(f"Download : {download_url}")
    if short_url:
        print(f"Short URL: {short_url}")


if __name__ == "__main__":
    main()
