
package com.bee.platform.datadriver.service;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.datadriver.entity.ErpCrmWorkPromote;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.datadriver.rq.WorkPromoteSaveRQ;

import java.util.List;

/**
 * <p>
 * 工作推进 服务类
 * </p>
 *
 * @author chenjie123123
 * @since 2019-06-24
 */
public interface ErpCrmWorkPromoteService extends IService<ErpCrmWorkPromote> {

    /**
     * 查询工作推进
     * @param commercialId
     * @return
     */
    List<ErpCrmWorkPromote> listCrmWorkPromote(Integer commercialId);

    /**
     * 新增工作推进
     * @param rq
     * @param userInfo
     * @return
     */
    ResponseResult<Integer> addCrmWorkPromote(WorkPromoteSaveRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 删除工作推进
     * @param id
     * @param userInfo
     * @return
     */
    ResponseResult<Integer> deleteCrmWorkPromote(Integer id, AuthPlatformUserInfo userInfo);
}
