package com.bee.platform.user.service;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.user.dto.IndexEnterpriseDTO;
import com.bee.platform.user.dto.TodoListDTO;

/**
 * @Description 首页展示接口信息
 * @author chenxm66777123
 * @Date 2019年5月6日
 * @version 1.0.0
 */
public interface HomeService {

	/**
	 * @Description 统计工作台待办任务列表
	 * @author chenxm66777123
	 * @Date 2019年5月5日
	 * @version 1.0.0
	 */
	ResponseResult<TodoListDTO> countTodoList(AuthPlatformUserInfo userInfo);

	/**
	 * @Description 统计当前登录企业信息（成员总数-部门总数）
	 * @author chenxm66777123
	 * @Date 2019年5月6日
	 * @version 1.0.0
	 */
	ResponseResult<IndexEnterpriseDTO> countEnterpriseInfo(AuthPlatformUserInfo userInfo);

}
