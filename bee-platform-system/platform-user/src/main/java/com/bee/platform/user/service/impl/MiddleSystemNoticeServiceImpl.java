package com.bee.platform.user.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.entity.*;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.MiddleNoticeUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.user.dao.mapper.MiddleSystemNoticeMapper;
import com.bee.platform.user.dto.MiddleSystemNoticeDTO;
import com.bee.platform.user.entity.MiddleSystemNotice;
import com.bee.platform.user.rq.MiddleSystemNoticeRQ;
import com.bee.platform.user.service.MiddleSystemNoticeService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

/**
 * <p>
 * 中台系统通知 服务实现类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-09
 */
@Slf4j
@Service
public class MiddleSystemNoticeServiceImpl extends ServiceImpl<MiddleSystemNoticeMapper, MiddleSystemNotice> implements MiddleSystemNoticeService {

    /**
     * 根据用户信息查询所有中台系统通知
     * @param userInfo 用户信息
     * @return 中台系统通知列表
     */
    @Override
    public ResponseResult<List<MiddleSystemNoticeDTO>> getMiddleSystemNoticeList(AuthPlatformUserInfo userInfo,Integer type ,Page page) {
        Pagination pagination = PageUtils.transFromPage(page);
        Integer userId = userInfo.getId();
        Wrapper wrapper = new EntityWrapper<MiddleSystemNotice>()
                .eq("notifier_id", userId)
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey())
                .orderBy("is_read",true).orderBy("create_time", false);
        if (type.equals(1)){
            wrapper = wrapper.and().eq("is_read",0);
        }
        List<MiddleSystemNotice> list = baseMapper.selectPage(pagination, wrapper);

        List<MiddleSystemNoticeDTO> dto = BeanUtils.assemble(MiddleSystemNoticeDTO.class, list);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto,PageUtils.transToPage(pagination));
    }

    /**
     * 修改通知阅读状态
     * @param userInfo 用户信息
     * @param id id
     * @return 修改结果
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public ResponseResult updateNoticeById(AuthPlatformUserInfo userInfo, Long id) {
        Integer userId = userInfo.getId();
        if(!update(new MiddleSystemNotice().setIsRead(1), new EntityWrapper<MiddleSystemNotice>()
                .eq("id", id).eq("notifier_id", userId))){
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.MIDDLE_SYSTEM_NOTICE_UPDATE_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 中台系统通知全部置为已读
     * @param userInfo 用户信息
     * @return 操作结果
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public ResponseResult updateAllNotice(AuthPlatformUserInfo userInfo) {
        Integer userId = userInfo.getId();
        List<MiddleSystemNotice> list = selectList(new EntityWrapper<MiddleSystemNotice>().eq("notifier_id", userId).eq("is_read", 0));
        if(CollectionUtils.isNotEmpty(list)){
            List<Long> ids = list.stream().map(MiddleSystemNotice::getId).collect(Collectors.toList());
            if(!update(new MiddleSystemNotice().setIsRead(1),new EntityWrapper<MiddleSystemNotice>()
                    .eq("notifier_id",userId)
                    .in("id",ids))){
                throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.MIDDLE_SYSTEM_NOTICE_UPDATE_FAILED);
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 创建中台系统通知消息
     * @param rq 请求参数
     * @return 操作结果
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public ResponseResult saveNotice(MiddleSystemNoticeRQ rq) {
        Date time=new Date();
        MiddleSystemNotice notice = BeanUtils.copyProperties(rq, MiddleSystemNotice.class).setIsRead(0)
                .setStatus(EnumCommon.LogicStatus.NORMAL.getKey()).setCreateTime(time).setUpdateTime(time);
        if(!insert(notice)){
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.MIDDLE_SYSTEM_NOTICE_SAVE_FAILED);

        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }


    /**
     * 创建中台系统通知
     * @param notifierId 通知人id
     * @param title 标题
     * @param noticeTemplateKey 通知模板key
     * @param param  参数
     * @return 操作结果
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public ResponseResult createNotice(Integer notifierId, String title, Integer noticeTemplateKey, Object... param) {
        String content = MiddleNoticeUtils.getValue(noticeTemplateKey, param);
        Date time=new Date();
        MiddleSystemNotice notice = new MiddleSystemNotice().setContent(content).setTitle(title).setNotifierId(notifierId)
                .setIsRead(0).setStatus(EnumCommon.LogicStatus.NORMAL.getKey()).setCreateTime(time).setUpdateTime(time);
        log.info("创建中台系统通知");
        if(!insert(notice)){
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.MIDDLE_SYSTEM_NOTICE_SAVE_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }
}
