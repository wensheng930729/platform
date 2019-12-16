package com.bee.platform.user.service;

import org.springframework.web.multipart.MultipartFile;

import com.alibaba.fastjson.JSONObject;
import com.bee.platform.common.entity.ResponseResult;

/**
 * @Description 华为云对象存储相关接口
 * @author chenxm66777123
 * @Date 2019年5月9日
 * @version 1.0.0
 */
public interface OBSFileService {

	/**
	 * @Description 
	 * @author chenxm66777123
	 * @Date 2019年5月9日
	 * @version 1.0.0
	 */
	ResponseResult<JSONObject> upload(MultipartFile file,String fileExtention);
	

}
