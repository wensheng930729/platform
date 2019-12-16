package com.bee.platform.user.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.*;
import com.bee.platform.user.dto.FeedbackDTO;
import com.bee.platform.user.dto.FeedbackDetailDTO;
import com.bee.platform.user.dto.FeedbackRecordDTO;
import com.bee.platform.user.entity.Feedback;
import com.bee.platform.user.rq.FeedbackRQ;

import java.util.List;

/**
 * <p>
 * 意见反馈 服务类
 * </p>
 *
 * @author liliang123
 * @since 2019-04-28
 */
public interface FeedbackService extends IService<Feedback> {

    /**
     * 查询所有意见反馈列表
     *
     * @param page 分页参数
     * @return
     */
    ResponseResult<List<FeedbackDTO>> getAll(Page page);

    /**
     * 查询反馈记录
     *
     * @param page
     * @return
     */
    ResponseResult<List<FeedbackRecordDTO>> getRecord(Page page);

    /**
     * 根据id查询 意见反馈详情
     *
     * @param id
     * @return
     */
    ResponseResult<FeedbackDetailDTO> getDetailById(Integer id);

    /**
     * 增加意见反馈
     *
     * @param rq 意见反馈入参
     * @return
     */
    ResponseResult<ResCodeEnum> add(FeedbackRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 根据id删除
     *
     * @param id
     * @return
     */
    ResponseResult<ResCodeEnum> deleteById(Integer id, ManagerInfo managerInfo);

}
