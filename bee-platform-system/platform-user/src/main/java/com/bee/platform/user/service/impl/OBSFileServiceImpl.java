package com.bee.platform.user.service.impl;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.user.dto.QCloudDTO;
import com.bee.platform.user.service.OBSFileService;
import com.obs.services.ObsClient;
import com.obs.services.exception.ObsException;
import com.obs.services.model.HeaderResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.IOException;
import java.util.UUID;

/**
 * @Description 华为云对象存储相关接口实现类
 * @author chenxm66777123
 * @Date 2019年5月9日
 * @version 1.0.0
 */
@Slf4j
@Service
public class OBSFileServiceImpl implements OBSFileService {

	@Value("${huawei.obs.endPoint}")
	private String endPoint;
	@Value("${huawei.obs.ak}")
	private String ak;
	@Value("${huawei.obs.sk}")
	private String sk;
	@Value("${huawei.obs.bucketName}")
	private String bucketName;
	@Value("${huawei.obs.imageLocalpath}")
	private String imageLocalpath;

	/**
	 * @Description 通用文件上传
	 * @author chenxm66777123
	 * @Date 2019年5月9日
	 * @version 1.0.0
	 */
	@Override
	public ResponseResult<JSONObject> upload(MultipartFile file, String fileExtention) {
		// 本机运行开启
		// imageLocalpath = "F:/ImgTest/";
		String fileUUid = UUID.randomUUID().toString().replace("-", "");
		File tempFile = new File(imageLocalpath + fileUUid + fileExtention);
		if (!tempFile.exists()) {
			try {
				tempFile.createNewFile();
				file.transferTo(tempFile);
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		return this.fileToOBS(tempFile, fileUUid, fileExtention, true);
	}

	/**
	 * @Description 文件上传到华为云对象存储服务器上
	 * @author chenxm66777123
	 * @Date 2019年4月17日
	 * @version 1.0.0
	 */
	public ResponseResult<JSONObject> fileToOBS(File tempFile, String fileUUid, String fileExtention,
			boolean openDelete) {
		JSONObject json = new JSONObject();
		// 您的工程中可以只保留一个全局的ObsClient实例
		// ObsClient是线程安全的，可在并发场景下使用
		ObsClient obsClient = null;
		// ak ----- Y5C8SVCPHA3SUGK0FTQK
		// sk --- xIVglSyrKMkiq8mmRxHQZT7rfqCIcBguM8LcsajA
		try {
			// 创建ObsClient实例
			obsClient = new ObsClient(ak, sk, endPoint);
			// 调用接口进行操作，例如上传对象
			HeaderResponse response = obsClient.putObject(bucketName, imageLocalpath + fileUUid + fileExtention,
					tempFile); // localfile为待上传的本地文件路径，需要指定到具体的文件名
			JSONObject responseJson = JSONObject.parseObject(JSON.toJSONString(response));
			// 获取文件上传的地址
			String objectUrl = responseJson.getString(ConstantsUtil.OBJECTURL);
			String etag = responseJson.getString(ConstantsUtil.ETAG);

			// 组装成为腾讯云一样的参数类型,并返回
			QCloudDTO qCloudDTO = new QCloudDTO();
			qCloudDTO.setVid(etag);
			qCloudDTO.setAccess_url(objectUrl);
			qCloudDTO.setResource_path(objectUrl);
			qCloudDTO.setSource_url(objectUrl);
			qCloudDTO.setUrl(objectUrl);

			json = JSONObject.parseObject(JSON.toJSONString(qCloudDTO));

		} catch (ObsException e) {
			log.info("Response Code {}" + e.getResponseCode());
			log.info("Error Message {}" + e.getErrorMessage());
			log.info("Error Code {}" + e.getErrorCode());
			log.info("Request ID {}" + e.getErrorRequestId());
			log.info("Host ID {}" + e.getErrorHostId());
		} finally {
			// 关闭ObsClient实例，如果是全局ObsClient实例，可以不在每个方法调用完成后关闭
			// ObsClient在调用ObsClient.close方法关闭后不能再次使用
			if (obsClient != null) {
				try {
					obsClient.close();
				} catch (IOException e) {
				}
			}
			
			if (openDelete && tempFile.exists()) {
				tempFile.delete();
			}
		}
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, json);

	}

}
