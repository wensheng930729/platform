package com.bee.platform.user.service.impl;

import java.util.Date;
import java.util.List;

import org.apache.commons.compress.utils.Lists;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.constants.enums.EnumCheckStatus;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.constants.enums.EnumFeedbackType;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ManagerInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.user.dao.mapper.FeedbackMapper;
import com.bee.platform.user.dto.FeedbackDTO;
import com.bee.platform.user.dto.FeedbackDetailDTO;
import com.bee.platform.user.dto.FeedbackRecordDTO;
import com.bee.platform.user.entity.Feedback;
import com.bee.platform.user.rq.FeedbackRQ;
import com.bee.platform.user.rq.FileRQ;
import com.bee.platform.user.service.FeedbackService;

import lombok.extern.slf4j.Slf4j;

/**
 * <p>
 * 意见反馈 服务实现类
 * </p>
 *
 * @author liliang123
 * @since 2019-04-28
 */
@Slf4j
@Service
public class FeedbackServiceImpl extends ServiceImpl<FeedbackMapper, Feedback> implements FeedbackService {


    @Autowired
    private FeedbackMapper feedbackMapper;

    @Override
    public ResponseResult<List<FeedbackDTO>> getAll(Page page) {
        Pagination pagination = PageUtils.transFromPage(page);
        List<Feedback> feedbackList = feedbackMapper.selectPage(pagination, new EntityWrapper<>(new Feedback()
                .setStatus(EnumCommon.IsActive.is_active.getKey())).orderBy("create_time",false));
        if (CollectionUtils.isEmpty(feedbackList)) {
            log.info("未找到相关反馈信息，{0}中的{1}", "FeedbackServiceImpl", "getAll");
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, Lists.newArrayList(), PageUtils.transToPage(pagination));
        }
        List<FeedbackDTO> feedbackListDTO = BeanUtils.assemble(FeedbackDTO.class, feedbackList);
        feedbackListDTO.forEach(dto -> {
            if (dto.getAdviceType().equals(EnumFeedbackType.adviceType.bug.getKey())) {
                dto.setAdviceTypeString(EnumFeedbackType.adviceType.bug.getValue());
            } else {
                dto.setAdviceTypeString(EnumFeedbackType.adviceType.advice.getValue());
            }
        });
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, feedbackListDTO, PageUtils.transToPage(pagination));
    }

    @Override
    public ResponseResult<List<FeedbackRecordDTO>> getRecord(Page page) {
        Pagination pagination = PageUtils.transFromPage(page);
        List<Feedback> feedbackList = feedbackMapper.selectPage(pagination, new EntityWrapper<>(new Feedback()
                .setStatus(EnumCommon.IsActive.is_active.getKey())).orderBy("create_time", false));
        List<FeedbackRecordDTO> feedbackRecordDTOS = BeanUtils.assemble(FeedbackRecordDTO.class, feedbackList);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,feedbackRecordDTOS, PageUtils.transToPage(pagination));
    }

    @Override
    public ResponseResult<FeedbackDetailDTO> getDetailById(Integer id) {
        Feedback feedback = feedbackMapper.selectOne(new Feedback().setStatus(EnumCommon.IsActive.is_active.getKey()).setId(id));
        if (ObjectUtils.isEmpty(feedback)) {
            log.info("未找到相关反馈信息，{0}中的{1}", "FeedbackServiceImpl", "getDetailById");
            return ResponseResult.buildResponseResult(ResCodeEnum.FEEDBACK_NOT_FIND);
        }
        FeedbackDetailDTO detailDTO = BeanUtils.copyProperties(feedback, FeedbackDetailDTO.class);
        // 把附件string转换为List
        List<FileRQ> files = JSON.parseArray(feedback.getFiles(), FileRQ.class);
        detailDTO.setFiles(files);
        if (detailDTO.getAdviceType().equals(EnumFeedbackType.adviceType.bug.getKey())) {
            detailDTO.setAdviceTypeString(EnumFeedbackType.adviceType.bug.getValue());
        } else {
            detailDTO.setAdviceTypeString(EnumFeedbackType.adviceType.advice.getValue());
        }
        // 修改该反馈的查看zhuangt
        feedback.setIsCheck(EnumCheckStatus.checkStatus.yes.getKey());
        if (feedbackMapper.updateById(feedback) != 1) {
            log.info("修改反馈查看状态失败，{0}中的{1}", "FeedbackServiceImpl", "getDetailById");
            return ResponseResult.buildResponseResult(ResCodeEnum.DELETE_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, detailDTO);
    }


    @Override
    public ResponseResult<ResCodeEnum> add(FeedbackRQ rq, AuthPlatformUserInfo userInfo) {
        Feedback feedback = BeanUtils.copyProperties(rq, Feedback.class);
        feedback.setStatus(EnumCommon.IsActive.is_active.getKey())
                .setFiles(JSON.toJSONString(rq.getFiles()))
                .setIsCheck(EnumCheckStatus.checkStatus.no.getKey())
                .setAdviceUser(userInfo.getName())
                .setCreateTime(new Date());
        if (feedbackMapper.insert(feedback) != 1) {
            log.error("增加意见反馈失败，{0}中的{1}", "FeedbackServiceImpl", "add");
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> deleteById(Integer id, ManagerInfo managerInfo) {
        Feedback feedback = feedbackMapper.selectOne(new Feedback().setId(id));
        if (ObjectUtils.isEmpty(feedback)) {
            log.info("未找到相关反馈信息，{0}中的{1}", "FeedbackServiceImpl", "deleteById");
            return ResponseResult.buildResponseResult(ResCodeEnum.FEEDBACK_NOT_FIND);
        }
        feedback.setStatus(EnumCommon.IsActive.not_active.getKey());
        if (feedbackMapper.updateById(feedback) != 1) {
            return ResponseResult.buildResponseResult(ResCodeEnum.DELETE_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

}
