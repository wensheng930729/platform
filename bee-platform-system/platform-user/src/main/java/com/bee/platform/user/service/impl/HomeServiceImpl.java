package com.bee.platform.user.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.bee.platform.common.constants.enums.EnumWorkOrders;
import com.bee.platform.common.constants.enums.EnumWorkbenchTask;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.user.authority.entity.AuthPlatformUserEnterprise;
import com.bee.platform.user.authority.service.AuthPlatformUserEnterpriseService;
import com.bee.platform.user.dao.mapper.DepartmentsMapper;
import com.bee.platform.user.dao.mapper.WorkOrdersMapper;
import com.bee.platform.user.dao.mapper.WorkbenchTaskMapper;
import com.bee.platform.user.dto.IndexEnterpriseDTO;
import com.bee.platform.user.dto.TodoListDTO;
import com.bee.platform.user.entity.Departments;
import com.bee.platform.user.entity.WorkOrders;
import com.bee.platform.user.entity.WorkbenchTask;
import com.bee.platform.user.service.HomeService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * @Description 首页展示接口信息实现类
 * @author chenxm66777123
 * @Date 2019年5月6日
 * @version 1.0.0
 */
@Service
public class HomeServiceImpl implements HomeService {
	@Autowired
	private WorkbenchTaskMapper workbenchTaskMapper;

	@Autowired
	private WorkOrdersMapper workOrdersMapper;

	@Autowired
	private AuthPlatformUserEnterpriseService platformUserEnterpriseService;

	@Autowired
	private DepartmentsMapper departmentsMapper;
	
	/**
	 * @Description 统计当前登录企业信息
	 * @author chenxm66777123
	 * @Date 2019年5月6日
	 * @version 1.0.0
	 */
	@Override
	public ResponseResult<TodoListDTO> countTodoList(AuthPlatformUserInfo userInfo) {
		TodoListDTO todoListDTO = new TodoListDTO();

		// 统计工作台待办任务列表
		int countTodoWorkbenchTask = workbenchTaskMapper.selectCount(
				new EntityWrapper<>(new WorkbenchTask().setTaskStatu(EnumWorkbenchTask.TASK_STATUS.todo.getKey())
						.setCreateId(userInfo.getId().longValue()).setStatus(Status.TRUE.getKey())));
		// 待办数量
		todoListDTO.setCountTodoWorkbenchTask(countTodoWorkbenchTask);

		// 统计工单待办事项
		int countTodoWorkOrders = workOrdersMapper.selectCount(
				new EntityWrapper<>(new WorkOrders().setOrderStatus(EnumWorkOrders.WORK_ORDER_STATUS.confirm.getKey())
						.setCreateId(userInfo.getId().longValue()).setStatus(Status.TRUE.getKey())));
		// 待办数量
		todoListDTO.setCountTodoWorkOrders(countTodoWorkOrders);
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, todoListDTO);
	}

	/**
	 * @Description 统计当前登录企业信息（成员总数-部门总数）
	 * @author chenxm66777123
	 * @Date 2019年5月6日
	 * @version 1.0.0
	 */
	@Override
	public ResponseResult<IndexEnterpriseDTO> countEnterpriseInfo(AuthPlatformUserInfo userInfo) {
		IndexEnterpriseDTO indexEnterprise = new IndexEnterpriseDTO();
		// 统计当前登录企业，激活的成员总数
		/*int totalMembership = enterprisesUsersMapper.selectCount(new EntityWrapper<>(new EnterprisesUsers()
				.setEnterpriseId(userInfo.getOrgId()).setIsActive(EnumEnterpriseUser.ActiveType.is_active.getKey())));*/
		int totalMembership=platformUserEnterpriseService.selectCount(new EntityWrapper<AuthPlatformUserEnterprise>()
				.where("enterprise_id={0} and status =1 and deleted=0",userInfo.getOrgId()));
		indexEnterprise.setTotalMembership(totalMembership);
		// 统计当前登录企业，部门总数
		int totalDepartments = departmentsMapper
				.selectCount(new EntityWrapper<>(new Departments().setOrgId(userInfo.getOrgId())));

		indexEnterprise.setTotalDepartments(totalDepartments);

		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, indexEnterprise);
	}

}
