package com.bee.platform.user.controller;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.dto.IndexEnterpriseDTO;
import com.bee.platform.user.dto.TodoListDTO;
import com.bee.platform.user.service.HomeService;
import com.bee.platform.user.service.UsersService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.servlet.http.HttpServletRequest;

/**
 * @author chenxm66777123
 * @version 1.0.0
 * @Description 首页相关接口编写
 * @Date 2019年5月5日
 */
@RestController
@CrossOrigin(origins = "*")
@Api(value = "index", tags = "首页相关API")
@RequestMapping("/index")
public class HomeController {
	/*
	@Autowired
	private UsersService usersService;
	*/

	@Autowired
	private AuthPlatformUserService userService;

	@Autowired
	private HomeService homeService;

	@GetMapping(value = "/countTodoList")
	@ApiOperation(value = "首页统计待办事项（工作台任务-工单）", notes = "首页统计待办事项（工作台任务-工单）")
	public ResponseResult<TodoListDTO> countTodoList(HttpServletRequest request) {
		/*UserInfo userInfo = usersService.getUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request));
		if (ObjectUtils.isEmpty(userInfo)) {
			return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
		}*/
		AuthPlatformUserInfo userInfo = userService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
		if (ObjectUtils.isEmpty(userInfo)) {
			return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
		}
		return homeService.countTodoList(userInfo);
	}

	@GetMapping(value = "/countEnterpriseInfo")
	@ApiOperation(value = "首页统计企业信息（成员总数-部门总数）", notes = "首页统计企业信息（成员总数-部门总数）")
	public ResponseResult<IndexEnterpriseDTO> countEnterpriseInfo(HttpServletRequest request) {
		/*UserInfo userInfo = usersService.getUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request));
		if (ObjectUtils.isEmpty(userInfo)) {
			return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
		}*/
		AuthPlatformUserInfo userInfo = userService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
		if (ObjectUtils.isEmpty(userInfo)) {
			return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
		}
		return homeService.countEnterpriseInfo(userInfo);
	}
}
