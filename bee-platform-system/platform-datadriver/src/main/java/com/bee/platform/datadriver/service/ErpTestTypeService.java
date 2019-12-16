package com.bee.platform.datadriver.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.datadriver.entity.ErpTestType;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.datadriver.rq.TestReportTypeRQ;

import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 * 化验类型 服务类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
public interface ErpTestTypeService extends IService<ErpTestType> {

    /**
     * 分页查询化验类型列表
     * @param pagination
     * @return
     */
    List<ErpTestType> listErpTestType(String sysToken, Integer companyId, TestReportTypeRQ rq, Pagination pagination);

    /**
     * 删除化验类型
     * @param id
     * @return
     */
    ResponseResult<Integer> deleteTestType(AuthPlatformUserInfo userInfo, String id);

    /**
     * 保存化验单类型
     * @param userInfo
     * @param rq
     * @return
     */
    ResponseResult<Integer> saveTestType(AuthPlatformUserInfo userInfo, TestReportTypeRQ rq);

	/**
	 * 化验类型
	 * @param userId
	 * @return
	 */
	ResponseResult<List<ErpTestType>> getTestType(Integer userId,String sysToken);
}
