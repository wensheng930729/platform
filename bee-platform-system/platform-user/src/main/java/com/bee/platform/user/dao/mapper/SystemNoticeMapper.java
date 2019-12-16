package com.bee.platform.user.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.user.entity.SystemNotice;

import java.util.List;

/**
 * <p>
 *  Mapper 接口
 * </p>
 *
 * @author junyang.li123
 * @since 2019-05-06
 */
public interface SystemNoticeMapper extends BaseMapper<SystemNotice> {

    /**
     * @notes: 批量插入系统通知
     * @Author: junyang.li
     * @Date: 9:27 2019/5/7
     * @param list : 系统通知数组
     * @return: void
     */
    void insertAll(List<SystemNotice> list);
    /**
     * @notes: 查询指定用户未读的系统通知
     * @Author: junyang.li
     * @Date: 10:07 2019/5/7
     * @param notifierId : 通知人id
     * @param list : 校验的系统通知
     * @return: java.util.List<java.lang.Integer>
     */
    List<Integer> selectNoticeByNotifierId (int notifierId,List<Integer> list);
}
