package com.bee.platform.user.service.impl;

import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.user.dao.mapper.EnterprisesAttachmentMapper;
import com.bee.platform.user.entity.EnterprisesAttachment;
import com.bee.platform.user.rq.EnterprisesAttachmentRQ;
import com.bee.platform.user.service.EnterprisesAttachmentService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.validation.Valid;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

/**
 * <p>
 * 企业附件信息表 服务实现类
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-04-25
 */
@Service
public class EnterprisesAttachmentServiceImpl extends ServiceImpl<EnterprisesAttachmentMapper, EnterprisesAttachment> implements EnterprisesAttachmentService {


    /**
     * 保存企业附件
     *
     * @param userInfo 用户信息
     * @param rqs      附件信息
     * @return 附件id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public boolean saveEnterprisesAttachment(AuthPlatformUserInfo userInfo, @Valid  List<EnterprisesAttachmentRQ> rqs) {

        Integer createId =  userInfo.getId();
        String creator = userInfo.getNickname();
        Date date = new Date();
        List<EnterprisesAttachment> attachmentList = BeanUtils.assemble(EnterprisesAttachment.class, rqs)
                .stream()
                .map(obj -> obj.setCreateId(createId)
                        .setCreator(creator)
                        .setCreateTime(date)
                        .setModifyId(createId)
                        .setModifier(creator)
                        .setModifyTime(date)
                        .setStatus(EnumCommon.LogicStatus.NORMAL.getKey()))
                .collect(Collectors.toList());
            if(!insertBatch(attachmentList)){
                throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ENTERPRISES_ATTACHMENT_SAVE_FAILED);
            }
        return true;
    }
}
