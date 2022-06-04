from collections import namedtuple
from sklearn.cluster import DBSCAN
import numpy as np
import cv2


class CharacterCluster:
    """Performs DBSCAN clustering for characters

    ----------
    :param metric : string,
        DBSCAN metric param
    :param eps : float,
        DBSCAN eps param
    :param n_jobs : int,
        DBSCAN n_jobs param
    :param image_additional_padding : int,
        Images before fitting them into to the DBSCAN are aligned with padding to the biggest one.
        After it to the each dimension is added this value
    """
    def __init__(self, metric, eps, n_jobs, image_additional_padding):
        self.metric = metric
        self.eps = eps
        self.n_jobs = n_jobs
        self.image_additional_padding = image_additional_padding
        self.ReadImage = namedtuple("ReadImage", ["image_path", "image"])
        self.ImageEncoding = namedtuple("ImageEncoding", ["image_path", "encoding"])
        self.max_height = 0
        self.max_width = 0

    def cluster_images(self, images):
        """Performs clustering for given images

        ----------
        :param images: array of strings,
            List with paths to images to cluster
        :return: dict,
            Dictionary with clusters ({cluster id} : {cluster elements})
        """
        encoded_images = self._encode_images(images)
        raw_encodings = [image.encoding for image in encoded_images]

        print("\nClustering with metric: {}, eps: {}, n_jobs: {} ..."
              .format(self.metric, self.eps, self.n_jobs))
        model = DBSCAN(
            metric=self.metric,
            eps=self.eps,
            n_jobs=self.n_jobs,
            min_samples=1
        )
        model.fit(raw_encodings)
        print("Clustering done!")
        clusters = model.labels_
        number_of_clusters = len(np.unique(clusters))
        print("Found {} clusters!\n".format(number_of_clusters))

        grouped_clustering = {}
        for index, image in zip(clusters, encoded_images):
            grouped_clustering.setdefault(index, []).append(image)

        return grouped_clustering

    def _encode_images(self, images):
        number_of_images = len(images)
        print("Reading images... Images to read: {}".format(number_of_images))
        read_images = [self._read_image(image, index, number_of_images) for index, image in enumerate(images)]
        print("Reading done!")

        print("Encoding images... Images to encode: {}".format(number_of_images))
        encoded_images = [self._encode_image(image, index, number_of_images) for index, image in enumerate(read_images)]
        print("Encoding done!")

        return encoded_images

    def _read_image(self, image_path, index, number_of_images):
        print("Reading image: {} | {} / {}".format(image_path, index + 1, number_of_images))

        image = cv2.imread(image_path, 0)
        self.max_height = max(self.max_height, image.shape[0])
        self.max_width = max(self.max_width, image.shape[1])

        return self.ReadImage(image_path, image)

    def _encode_image(self, read_image, index, number_of_images):
        print("Encoding image: {} | {} / {}".format(read_image.image_path, index + 1, number_of_images))

        final_height = self.max_height + self.image_additional_padding
        final_width = self.max_width + self.image_additional_padding
        resized_image = cv2.copyMakeBorder(
            read_image.image,
            (final_height - read_image.image.shape[0] + 1) // 2,
            (final_height - read_image.image.shape[0]) // 2,
            (final_width - read_image.image.shape[1] + 1) // 2,
            (final_width - read_image.image.shape[1]) // 2,
            cv2.BORDER_CONSTANT,
            value=255
        )
        encoding = resized_image.flatten()

        return self.ImageEncoding(read_image.image_path, encoding)
