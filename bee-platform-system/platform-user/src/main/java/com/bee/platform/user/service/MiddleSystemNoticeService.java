package com.bee.platform.user.service;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.user.dto.MiddleSystemNoticeDTO;
import com.bee.platform.user.entity.MiddleSystemNotice;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.user.rq.MiddleSystemNoticeRQ;

import java.util.List;

/**
 * <p>
 * 中台系统通知 服务类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-09
 */
public interface MiddleSystemNoticeService extends IService<MiddleSystemNotice> {
    /**
     * 根据用户信息查询所有中台系统通知
     * @param userInfo 用户信息
     * @param type 业务类型
     * @param page 分页对象
     * @return
     */
    ResponseResult<List<MiddleSystemNoticeDTO>> getMiddleSystemNoticeList(AuthPlatformUserInfo userInfo, Integer type, Page page);

    /**
     * 修改通知阅读状态
     * @param userInfo 用户信息
     * @param id id
     * @return 操作结果
     */
    ResponseResult updateNoticeById(AuthPlatformUserInfo userInfo, Long id);

    /**
     * 中台系统通知全部置为已读
     * @param userInfo 用户信息
     * @return 操作结果
     */
    ResponseResult updateAllNotice(AuthPlatformUserInfo userInfo);

    /**
     * 创建中台系统通知消息
     * @param rq 请求参数
     * @return 操作结果
     */
    ResponseResult saveNotice(MiddleSystemNoticeRQ rq);

    /**
     * 创建中台系统通知
     * @param notifierId 通知人id
     * @param title 标题
     * @param noticeTemplateKey 通知模板key
     * @param param  参数
     * @return 操作结果
     */
    ResponseResult createNotice(Integer notifierId,String title,Integer noticeTemplateKey,Object...param);


}
