package com.bee.platform.user.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.NoticeTemplate;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.NoticeTemplateType;
import com.bee.platform.user.dto.CreateNoticesDTO;
import com.bee.platform.user.dto.SystemNoticeDTO;
import com.bee.platform.user.entity.SystemNotice;
import com.baomidou.mybatisplus.service.IService;

import java.util.List;

/**
 *  后台管理系统通知相关接口
 *
 * @author junyang.li
 * @since 2019-05-06
 */
public interface SystemNoticeService extends IService<SystemNotice> {
    /**
     * @notes: 列表查询系统通知
     * @Author: junyang.li
     * @Date: 10:29 2019/5/6
     * @param managerId : 当前用户id
     * @param pagination : 分页对象
     * @return: com.bee.platform.common.entity.ResponseResult
     */
    ResponseResult<List<SystemNoticeDTO>> getNoticeList(int managerId,Integer type, Pagination pagination);
    /**
     * @notes: 阅读系统通知
     * @Author: junyang.li
     * @Date: 10:39 2019/5/6
     * @param managerId : 当前用户id
     * @param noticeIds : 系统通知id数组
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    ResponseResult<ResCodeEnum> updateNotices(int managerId,List<Integer> noticeIds);
    /**
     * @notes: 新增系统通知并插入到数据库中
     * @Author: junyang.li
     * @Date: 17:37 2019/5/6
     * @param dto : 参数对象
     */
    void createNotices(CreateNoticesDTO dto);
    /**
     * @notes:  生成并插入一个系统通知
     * @Author: junyang.li
     * @Date: 14:59 2019/5/10
     * @param notifierIds : 通知人id
     * @param templateType : 通知模板类型
     * @param params : 参数
     * @return: void
     */
    void insertNotice(int notifierIds,NoticeTemplateType templateType,Object...params);
    /**
     * @notes: 创建新的系统通知并返回一个通知对象
     * @Author: junyang.li
     * @Date: 14:17 2019/5/9
     * @param notifierIds : 通知人id
     * @param templateType : 模板类型
     * @param params : 参数集合
     * @return: com.bee.platform.user.entity.SystemNotice
     */
    SystemNotice createNotice(int notifierIds,NoticeTemplateType templateType,Object...params);
    /**
     * @notes: 创建新的系统通知并返回 通知对象集合
     * @Author: junyang.li
     * @Date: 14:17 2019/5/9
     * @param notifierIds : 通知人id
     * @param templateType : 模板类型
     * @param params : 参数集合
     * @return: java.util.List<com.bee.platform.user.entity.SystemNotice>
     */
    List<SystemNotice> createNotice(int[] notifierIds,NoticeTemplateType templateType,Object...params);
    /**
     * @notes: 批量插入系统通知
     * @Author: junyang.li
     * @Date: 13:42 2019/5/9
     * @param notices : 通知集合
     * @return: void
     */
    void insertAll(List<SystemNotice> notices);
}
