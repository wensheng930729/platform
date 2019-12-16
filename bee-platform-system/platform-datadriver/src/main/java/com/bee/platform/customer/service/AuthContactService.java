package com.bee.platform.customer.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.customer.rq.AuthContactAddRQ;
import com.bee.platform.customer.dto.AuthContactDto;
import com.bee.platform.customer.entity.AuthContact;
import com.bee.platform.customer.rq.AuthContactSelectRQ;
import com.bee.platform.customer.rq.AuthContactUpdateRQ;

import java.util.List;

/**
 * <p>
 *  服务类
 * </p>
 *
 * @author hongchuan.He
 * @since 2019-05-20
 */
public interface AuthContactService extends IService<AuthContact> {
    /**
     * 联系人添加
     * @param rqList
     * @return
     */
    ResponseResult<ResCodeEnum> addContact(List<AuthContactAddRQ> rqList);
    /**
     * 批量删除
     * @param ids
     * @return
     */
    ResponseResult<ResCodeEnum> deleteByIds(String ids);
    /**
     * 更新联系人
     * @param rq
     * @return
     */
    ResponseResult<ResCodeEnum> updateContact(AuthContactUpdateRQ rq);

    /**
     * 条件查询列表
     * @param rq
     * @param page
     * @return
     */
    ResponseResult<List<AuthContactDto>> getList(AuthContactSelectRQ rq, Page page);


}
